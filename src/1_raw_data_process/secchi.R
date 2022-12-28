library(lubridate)
library(tidyverse)

# Updated 2022-12-28

#################### LOAD DATA ####################
inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/29/5a5a5606737d760b61c43bc59460ccc9"
infile1 <- tempfile()
download.file(inUrl1, infile1, method = "libcurl")
LTERsecchi <- read_csv(infile1)

# get ice on/off dates
ice0 = read_csv("Data/derived/ntl_icedatescombo.csv") |> 
  mutate(year4 = year(lastice)) |> 
  filter(year4 >= 1980) |> 
  select(-year)

secchi = LTERsecchi |> select(lakeid:sampledate, secnview, ice) |> 
  filter(!is.na(secnview)) |> 
  left_join(ice0) |> 
  mutate(iceOn = if_else(sampledate < lastice, TRUE, FALSE)) |> 
  mutate(iceOn = if_else(is.na(iceOn) & month(sampledate) <= 3, TRUE, iceOn)) |> 
  mutate(iceOn = if_else(is.na(iceOn) & month(sampledate) > 3, FALSE, iceOn)) |> 
  group_by(lakeid, year4) |> 
  mutate(n = n()) |> 
  filter(!n < 10) |> # filter out low year
  ungroup() |> select(-n)
  
# s.all = secchi |> 
#   group_by(lakeid, year4) %>% 
#   slice_max(secnview, with_ties = FALSE, n = 1) %>% # if ties, select the first 
#   mutate(metric = "secchi_all_max") %>% 
#   select(lakeid, metric, sampledate, year4, daynum, secnview)

s.openwater.max = secchi |> 
  filter(iceOn == FALSE) |> 
  group_by(lakeid, year4) %>% 
  slice_max(secnview, with_ties = FALSE, n = 1) %>% # if ties, select the first 
  mutate(metric = "secchi_openwater_max") %>% 
  select(lakeid, metric, sampledate, year4, daynum, secnview)

s.openwater.min = secchi |> 
  filter(iceOn == FALSE) |> 
  group_by(lakeid, year4) %>% 
  slice_min(secnview, with_ties = FALSE, n = 1) %>% # if ties, select the first 
  mutate(metric = "secchi_openwater_min") %>% 
  select(lakeid, metric, sampledate, year4, daynum, secnview)

secchi.out = s.openwater.max |> bind_rows(s.openwater.min) |> select(-secnview) |> 
  rename(year = year4)

write_csv(secchi.out, "Data/final_metric_files/secchi.csv")
  