
physics <- function(path_in, path_out, path_out_derived) {
  # Load NTL LTER long term physical lake data
  # Package ID: knb-lter-ntl.29.29 Cataloging System:https://pasta.edirepository.org.
  # Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
  inUrl3 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/29/03e232a1b362900e0f059859abe8eb97"
  infile3 <- tempfile()
  download.file(inUrl3, infile3, method = "auto")
  
  dt0 <- read_csv(infile3) |> filter(!is.na(wtemp) & !is.na(o2))
  
  ############## Bad Data ###############
  # ME 2017-10-04 o2
  # ME 2007-05-14 wtemp
  # print(dt0 |> filter(lakeid == 'ME', sampledate == as.Date('2017-10-04')) |> select(lakeid,sampledate,wtemp,o2,o2sat),n=30)
  
  dt0 = dt0 |> mutate(o2 = if_else(lakeid == 'ME' & sampledate == as.Date('2017-10-04'), NA_real_, o2)) |> 
    mutate(wtemp = if_else(lakeid == 'ME' & sampledate == as.Date('2007-05-14'), NA_real_, wtemp))
  
  # View(dt0 |> filter(year4 == 2008) |> filter(lakeid %in% c('ME',"MO")))
  ############## ######## ###############
  
  # Only use common depths
  # usedepths = dt1 %>% group_by(depth) %>% tally() %>% filter(n >= 500) %>% pull(depth)
  # dt1 = dt1 %>% filter(depth %in% usedepths) %>% 
  #   filter(year4 >= 1982)
  
  get_dens <- function(temp, salt){
    dens = 999.842594 + (6.793952 * 10^-2 * temp) - (9.095290 * 10^-3 * temp^2) +
      (1.001685 * 10^-4 * temp^3) - (1.120083 * 10^-6 * temp^4) + (6.536336 * 10^-9 * temp^5) +
      (8.24493 * 10^-1 -4.0899 * 10^-3 * temp+ 7.6438 * 10^-5 * temp^2 - 8.2467 * 10^-7 * temp^3 + 
         5.3875 * 10^-9* temp^4) * salt+
      (-5.72466 *  10^-3 + 1.0227 * 10^-4 * temp -1.6546 * 10^-6 * temp^2) * salt^(3/2) +
      (4.8314*  10^-4 ) * salt
    return(dens)
  }
  
  # Load bathymetry
  bath <- read_csv(path_in) %>% 
    bind_rows(data.frame('lakeid' = rep('FI',2), 'Depth_m' = c(0,18.9),
                         'Depth_ft' = c(0,0),'area' = c(874000, 0)))
  
  # Make full grid of depths and dates
  useYears = dt0 %>% 
    group_by(lakeid, year4, sampledate) %>% tally() %>% 
    group_by(lakeid, year4) %>% tally() %>% filter(n >= 5) |> select(-n)
  
  useDepths = dt0 |> group_by(lakeid, depth) %>% tally() %>% filter(n >= 300) %>% select(lakeid, depth)
  
  useDates =  dt0 |> group_by(lakeid, sampledate) %>% tally() %>% 
    left_join(useDepths |> group_by(lakeid) |> summarise(depthN = n())) |> 
    filter(n > depthN*0.6) |> # use days that have 60% of depth measurements
    select(lakeid, sampledate)
  
  # Full grid of dates and depths
  fullDatesDepths = useDates |> full_join(useDepths) |> 
    mutate(year4 = year(sampledate)) |> 
    right_join(useYears)
  
  # Make original dataset fit to new grid
  
  dt1 = fullDatesDepths |> left_join(dt0)
  
  data.temp <- dt1 %>%
    mutate(wtemp = if_else(flagwtemp == 'K' & !is.na(flagwtemp), NA_real_, wtemp)) |> 
    group_by(lakeid, sampledate) %>%
    filter(sum(wtemp, na.rm = T) > 0) |> 
    fill(wtemp, .direction = 'up') %>%
    fill(wtemp, .direction = 'down') %>%
    mutate(iwtemp = na.approx(wtemp)) %>%
    mutate(wdens = get_dens(iwtemp, 0)) %>%
    select(lakeid, year = year4, sampledate, depth, iwtemp, wtemp, wdens)
  
  data.o2 <- dt1 %>%
    mutate(o2 = if_else(flago2 == 'K' & !is.na(flago2), NA_real_, o2)) |> 
    group_by(lakeid, sampledate) %>%
    filter(sum(o2, na.rm = T) > 0) |> 
    fill(o2, .direction = 'up') %>%
    fill(o2, .direction = 'down') %>%
    mutate(io2 = na.approx(o2)) %>%
    select(lakeid, year = year4, sampledate, depth, io2, o2)
  
  therm.list = list()
  strat.list = list()
  
  # Iterate for each lake
  for (name in unique(dt1$lakeid)){
    print(name)
    # Data for single lake
    data.temp.lake = data.temp %>% filter(lakeid == name)
    data.o2.lake = data.o2 %>% filter(lakeid == name) 
    
    # Often the top measurement and bottom measurement are high/low, which throws off
    # calculating density differences. Take median of top and bottom 2 meters. 
    maxDepth = max(data.temp.lake$depth, na.rm = T)
    if (max(data.temp.lake$depth, na.rm = T) > 10) {
      data.temp.lake = data.temp.lake |> 
        mutate(iwtemp = if_else(depth <= 2, median(iwtemp[depth <= 2]), iwtemp)) |> 
        mutate(iwtemp = if_else(depth >= (maxDepth-2), median(iwtemp[depth >= (maxDepth-2)]), iwtemp)) 
      
      data.o2.lake = data.o2.lake |>  
        mutate(o2 = if_else(depth <= 2, median(o2[depth <= 2]), o2)) |> 
        mutate(o2 = if_else(depth >= (maxDepth-2), median(o2[depth >= (maxDepth-2)]), o2)) 
    }
    
    
    #Get hyposometry for lake 
    hyp <- bath %>%
      filter(lakeid == name)
    if (max(data.temp.lake$depth) > max(hyp$Depth_m)){
      hyp <- rbind(hyp, hyp[nrow(hyp),])
      hyp$Depth_m[nrow(hyp)] <- max(data.temp.lake$depth)
    }
    
    df.lake <- data.temp.lake %>%
      group_by(year, sampledate) %>%
      distinct(depth, .keep_all = TRUE) %>%
      mutate(dup = duplicated(depth)) %>%
      summarise(thermdep = thermo.depth(wtr = iwtemp[which(dup == FALSE)], depths = depth[which(dup == FALSE)], 
                                Smin = 0.1, seasonal = TRUE, index = FALSE,
                                mixed.cutoff = 1),
                densdiff = wdens[which.max(depth)] - wdens[which.min(depth)],
                surfwtemp = iwtemp[which.min(depth)]) 
    
    # for winter?
    df.lake = df.lake %>% mutate(densdiff = ifelse(densdiff > 0.1 & surfwtemp >= 4, densdiff, NA)) %>% 
      mutate(thermdep = ifelse(is.na(thermdep), NA, thermdep))
    
    # export thermocline depth
    therm.list[[name]] = data.frame(lakeid = name, sampledate = df.lake$sampledate, thermdepth_m = df.lake$thermdep)
    
    
    dz = 0.5
    
    # Get energy 
    en <- data.temp.lake %>%
      group_by(year, sampledate) %>%
      arrange(depth) %>%
      summarise(z = seq(min(depth),max(depth),dz),
                area = approx(hyp$Depth_m, hyp$area, seq(min(depth), max(depth),dz))$y,
                density = approx(depth, wdens, seq(min(depth), max(depth),dz))$y,
                temp = approx(depth, wtemp, seq(min(depth), max(depth),dz))$y) %>%
      mutate('energy' = (area * dz) * density *temp * 4186,
             'n2' = c(0,buoyancy.freq(temp, z))) %>%
      summarise('energy' = sum(energy, na.rm = T)/max(area, na.rm = T),
                'n2max' = max(n2))
    
    df.lake <- df.lake[complete.cases(df.lake),]
    
    # Get oxygen
    an <- data.o2.lake %>%
      group_by(year, sampledate) %>%
      summarise(z = seq(min(depth),max(depth),dz),
                area = approx(hyp$Depth_m, hyp$area, seq(min(depth), max(depth),dz))$y,
                do = approx(depth, o2, seq(min(depth), max(depth), dz))$y) %>%
      # summarise('do' = abs(trapz(z * area, do)))
      summarise('do' = sum(dz * area * do))
    
    # Final stratification data.frame
    strat.df =  df.lake %>% group_by(year) %>%  
      summarise(lakeid = name, 
                straton = min(sampledate, na.rm = T), 
                stratoff = max(sampledate, na.rm = T), 
                duration = as.numeric(max(sampledate, na.rm = T) - min(sampledate, na.rm = T)))
                
    en.df = en %>% group_by(year) %>% 
      summarise(energy = sampledate[which.max(energy)],
                stability = sampledate[which.max(n2max)])
            
    # Get minimum anoxia after stratification 
    anoxia.df = an %>% ungroup() %>% left_join(strat.df %>% select(year, straton, stratoff)) %>% 
      group_by(year) %>% 
      filter(sampledate >= straton & sampledate <= stratoff) %>%
      summarise(minimum_oxygen =  sampledate[which.min(do)])
    
    # Join anoxia to strat dataframe
    strat.list[[name]] = strat.df %>% left_join(en.df) %>% left_join(anoxia.df)     
  }
  
  # Export thermocline depths
  therm.df = do.call(rbind.data.frame, therm.list)
  write_csv(therm.df, file = path_out_derived)
  
  # Export physics metrics
  strat.df = do.call(rbind.data.frame, strat.list)
  strat.df.wide = strat.df %>% select(-duration) %>% 
    pivot_longer(cols = straton:minimum_oxygen, names_to = "metric", values_to = "sampledate") %>% 
    mutate(daynum = yday(sampledate)) 
  write_csv(strat.df.wide, file = path_out)
  
  # Plot comparison with old data file
  # test = read_csv('Data/old/phenology_dates_v1.csv') %>% 
  #   rename(lakeid = id, metric=variable, daynum = value) %>% 
  #   filter(metric %in% c(unique(strat.df.wide$metric), "anoxia")) %>% 
  #   mutate(metric = ifelse(metric == "anoxia", "anoxia_summer", metric))
  # for (name in unique(dt1$lakeid)){
  #   p1 = ggplot(strat.df.wide %>% filter(lakeid == name)) +
  #      geom_point(aes(x = year, y = daynum)) +
  #       geom_line(aes(x = year, y = daynum)) +
  #       geom_point(data = test %>% filter(lakeid == name), aes(x = year, y = daynum), col = 'blue') +
  #       geom_line(data = test %>% filter(lakeid == name), aes(x = year, y = daynum), col = 'blue') +
  #       facet_wrap(~metric) +
  #       labs(title = name)
  #   print(p1)
  # }

  return(path_out)
  
}


