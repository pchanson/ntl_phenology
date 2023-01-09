
zoopDensity <- function(path_out) {

  #### Download southern lake zooplankton data from EDI ####
  inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/90/33/5880c7ba184589e239aec9c55f9d313b"
  infile1 <- tempfile()
  download.file(inUrl1, infile1, method = "curl")
  dt1 <- read_csv(infile1)
  
  #### Download northern lake zooplankton data from EDI ####
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/36/c4b652eea76cd431ac5fd3562b1837ee" 
  infile1 <- tempfile()
  download.file(inUrl1,infile1,method="curl")
  dt2 <-read_csv(infile1)
  
  # get ice on/off dates
  ice0 = read_csv("Data/final_metric_files/ice.csv") |> 
    filter(metric == 'iceoff') |> 
    select(lakeid, year4 = year, lastice = sampledate)
  
  #Combine files
  dt = dt1 |> select(-towdepth) |> 
    bind_rows(dt2) |> 
    left_join(ice0) |> 
    filter(sample_date > lastice) |> 
    mutate(code = floor(species_code/10000)) |>
    mutate(zoopGroup = case_when(code == 1 ~ 'copepod nauplii',
                                 code == 2 ~ 'copepod',
                                 code == 3 ~ 'calanoid',
                                 code == 4 ~ 'harpacticoid',
                                 code == 5 ~ 'cladocera',
                                 code == 6 ~ 'rotifer',
                                 code == 7 ~ 'unknown',
                                 code == 8 ~ 'unknown',
                                 code == 9 ~ 'unknown'))
  
  # Zooplankton ID codes
  codes = dt |> 
    group_by(species_code) |> 
    summarise(first(species_name))
  
  # by zoop group
  # cladocera and copepods
  zoopDensity.cc = dt |> filter(code %in% c(1,2,3,4,5)) |> 
    group_by(lakeid, year4, sample_date) |> 
    summarize(density = sum(density, na.rm = T)) |> 
    group_by(lakeid, year4) |> 
    slice_max(density) |> 
    mutate(metric = 'zoopDensity', daynum = yday(sample_date))
  # 
  # # all zoops
  # zoopDensity = dt |> 
  #   group_by(lakeid, year4, sample_date) |> 
  #   summarize(density = sum(density, na.rm = T)) |> 
  #   group_by(lakeid, year4) |> 
  #   slice_max(density) |> 
  #   mutate(metric = 'zoopDensity', daynum = yday(sample_date))
  # 
  # Combine datasets 
  zoop.out = zoopDensity.cc |> 
    select(lakeid, metric, sampledate = sample_date, year = year4, daynum)
  
  # Plot check
  ggplot(zoop.out) + 
    geom_density(aes(x = daynum, color = metric)) +
    facet_wrap(~lakeid, scales = 'free_y')
  
  write_csv(zoop.out, path_out)
  
 return(path_out) 
}
