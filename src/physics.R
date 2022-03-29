# packages
# install.packages('tidyr')
# install.packages('tidyverse')
# install.packages('rLakeAnalyzer')

library(tidyverse)
library(rLakeAnalyzer)
library(lubridate)
library(zoo) # for na.approx
library(pracma) #for function trapz

# remove everything from workspace
rm(list = ls())

# Load NTL LTER long term physical lake data
# Package ID: knb-lter-ntl.29.29 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
inUrl3 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/29/03e232a1b362900e0f059859abe8eb97"
infile3 <- tempfile()
download.file(inUrl3, infile3, method = "curl")

dt1 <- read_csv(infile3) 
# Only use common depths
usedepths = dt1 |> group_by(depth) |> tally() |> filter(n >= 500) |> pull(depth)
dt1 = dt1 |> filter(depth %in% usedepths)

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
bath <- read_csv('Data/NTLhypsometry.csv') |> 
  bind_rows(data.frame('lakeid' = rep('FI',2), 'Depth_m' = c(0,18.9),
                       'Depth_ft' = c(0,0),'area' = c(874000, 0)))

data.temp <- dt1 %>%
  group_by(lakeid, sampledate) %>%
  filter(is.na(flagwtemp), !is.na(wtemp)) %>%
  filter(sum(!is.na(wtemp))>1) %>%
  fill(wtemp, .direction = 'up') %>%
  fill(wtemp, .direction = 'down') %>%
  mutate(iwtemp = na.approx(wtemp)) %>%
  mutate(wdens = get_dens(iwtemp, 0)) %>%
  select(lakeid, year = year4, sampledate, depth, iwtemp, wtemp, wdens)

data.o2 <- dt1 %>%
  group_by(lakeid, sampledate) %>%
  filter(is.na(flagwtemp), !is.na(o2)) %>%
  filter(sum(!is.na(o2))>1) %>%
  fill(o2, .direction = 'up') %>%
  fill(o2, .direction = 'down') %>%
  mutate(io2 = na.approx(o2)) %>%
  select(lakeid, year = year4, sampledate, depth, io2, o2)

therm.list = list()
strat.list = list()

# Iterate for each lake
for (name in unique(dt1$lakeid)){
  print(name)
  # Some years only have a couple of samples
  useYears = data.temp |> filter(lakeid == name) |> 
    group_by(year, sampledate) |> tally() |> 
    group_by(year) |> tally() |> filter(n >= 5) |> pull(year)
  
  # Data for single lake
  data.temp.lake = data.temp |> filter(lakeid == name) |> filter(year %in% useYears)
  data.o2.lake = data.o2 |> filter(lakeid == name) |> filter(year %in% useYears)
  
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
  df.lake = df.lake %>% mutate(densdiff = ifelse(densdiff > 0.1 & surfwtemp >= 4, densdiff, NA)) |> 
    mutate(thermdep = ifelse(is.na(thermdep), NA, thermdep))
  
  # export thermocline depth
  therm.list[[name]] = data.frame(lakeid = name, sampledate = df.lake$sampledate, thermdepth_m = df.lake$thermdep)
  
  
  dz = 0.1
  
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
    group_by(year, sampledate, thermdep) %>%
    arrange(depth) %>%
    summarise(z = seq(min(depth),max(depth),dz),
              area = approx(hyp$Depth_m, hyp$area, seq(min(depth), max(depth),dz))$y,
              do = approx(depth, o2, seq(min(depth), max(depth), dz))$y) %>%
    summarise('do' = abs(trapz(1 * area, do)))
  
  # Final stratification data.frame
  strat.df =  df.lake |> group_by(year) |>  
    summarise(lakeid = name, 
              straton = min(sampledate, na.rm = T), 
              stratoff = max(sampledate, na.rm = T), 
              duration = as.numeric(max(sampledate, na.rm = T) - min(sampledate, na.rm = T)))
              
  en.df = en |> group_by(year) |> 
    summarise(energy = sampledate[which.max(energy)],
              stability = sampledate[which.max(n2max)])
          
  # Get minimum anoxia after stratification 
  anoxia.df = an |> ungroup() |> left_join(strat.df |> select(year, straton)) |> 
    group_by(year) |> 
    filter(sampledate >= straton) |> 
    summarise(anoxia_summer =  sampledate[which.min(do)])
  
  # Join anoixa to strat dataframe
  strat.list[[name]] = strat.df |> left_join(en.df) |> left_join(anoxia.df)     
}

# Export thermocline depths
therm.df = do.call(rbind.data.frame, therm.list)
# write.csv(therm.df, file ='Data/thermocline.csv', quote = F, row.names = F)

# Export physics metrics
strat.df = do.call(rbind.data.frame, strat.list)
strat.df.wide = strat.df |> select(-duration) |> 
  pivot_longer(cols = straton:anoxia_summer, names_to = "metric", values_to = "sampledate") |> 
  mutate(daynum = yday(sampledate)) 
# write_csv(strat.df.wide, "Data/final_metric_data/physics.csv")


test = read_csv('Data/final_metric_data/physics.csv')

for (name in unique(dt1$lakeid)){
  p1 = ggplot(strat.df.wide |> filter(lakeid == name)) +
     geom_point(aes(x = year, y = daynum)) +
      geom_line(aes(x = year, y = daynum)) +
      geom_point(data = test |> filter(lakeid == name), aes(x = year, y = daynum), col = 'blue') +
      geom_line(data = test |> filter(lakeid == name), aes(x = year, y = daynum), col = 'blue') +
      facet_wrap(~metric) +
      labs(title = name)
  print(p1)
}



