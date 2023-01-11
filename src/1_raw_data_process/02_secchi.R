
secchi <- function(ice_file, path_out) {
  # Updated 2022-12-28
  
  #################### LOAD DATA ####################
  inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/29/5a5a5606737d760b61c43bc59460ccc9"
  infile1 <- tempfile()
  download.file(inUrl1, infile1, method = "libcurl")
  LTERsecchi <- read_csv(infile1)
  
  ### Get TSI ###
  LTERsecchi |> filter(month(sampledate) %in% c(7,8)) |> 
    group_by(lakeid) |> 
    summarise(mean.Secchi = mean(secnview, na.rm = T)) |> 
    mutate(TSI.SD = 60-14.41*log(mean.Secchi))
  
  # TSI < 40 Oligotrophic
  # TSI 40-50 Mesotrophic
  # TSI >50 Eutrophic
  
  # get ice on/off dates
  ice0 = read_csv(ice_file) |> 
    filter(metric == 'iceoff') |> 
    select(lakeid, year4 = year, lastice = sampledate)
  
  secchi = LTERsecchi |> select(lakeid:sampledate, secnview, ice) |> 
    filter(!is.na(secnview)) |> 
    left_join(ice0) |> 
    filter(sampledate > lastice) |> 
    group_by(lakeid, year4) |> 
    mutate(n = n()) |> 
    filter(!n < 10) |> # filter out low year
    ungroup() |> select(-n)
  
  s.openwater.max = secchi |> 
    group_by(lakeid, year4) %>% 
    slice_max(secnview, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(metric = "secchi_max") %>% 
    select(lakeid, metric, sampledate, year4, daynum, secnview)
  
  s.openwater.min = secchi |> 
    group_by(lakeid, year4) %>% 
    slice_min(secnview, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(metric = "secchi_min") %>% 
    select(lakeid, metric, sampledate, year4, daynum, secnview)
  
  s.spring.max = secchi |> 
    filter(yday(sampledate) < 200) |>
    group_by(lakeid, year4) %>% 
    slice_max(secnview, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(metric = "secchi_springmax") %>% 
    select(lakeid, metric, sampledate, year4, daynum, secnview)
  
  
  secchi.out = s.openwater.max |> bind_rows(s.openwater.min) |> 
    bind_rows(s.spring.max) |> 
    select(-secnview) |> 
    rename(year = year4)
  
  write_csv(secchi.out, path_out)

  return(path_out) 
}
  