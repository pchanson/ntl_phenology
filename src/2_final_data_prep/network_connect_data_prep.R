# script to prep phenology event dates for network connectivity analysis
# CDB, 2022-04-19

library(tidyverse)

# read in data
dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v1.csv")

# metrics we want to analyze
metrics_want = c("iceoff", "straton", "chlor_spring", "daphnia_biomass", "secchi_openwater", "stability", "energy", "anoxia_summer", "chlor_fall", "doc", "stratoff", "iceon")
all_metrics = unique(dat$metric)
all_metrics[!(all_metrics %in% metrics_want)]

# create dictionaries for each node = lake-metric combination
dictionary_all = dat %>% 
  filter(metric %in% metrics_want) %>% 
  select(lakeid, metric) %>% 
  distinct() %>% 
  mutate(node = row_number())

dictionary_S = dat %>% 
  filter(metric %in% metrics_want & lakeid %in% c("FI", "ME", "MO", "WI")) %>% 
  select(lakeid, metric) %>% 
  distinct() %>% 
  mutate(node = row_number())

dictionary_N = dat %>% 
  filter(metric %in% metrics_want & !(lakeid %in% c("FI", "ME", "MO", "WI"))) %>% 
  select(lakeid, metric) %>% 
  distinct() %>% 
  mutate(node = row_number())

### prep the data to output
# all lakes
dat_all = dat %>% 
  filter(metric %in% metrics_want & year >= 1996) %>% 
  mutate(sampledate_fill = as.Date(paste0((year-1), "-12-31")) + daynum_fill)
min_date_all = min(dat_all$sampledate_fill)
dat_all = dat_all %>% 
  mutate(days_from_start = as.numeric(sampledate_fill - min_date_all)) %>% 
  left_join(dictionary_all)
dat_all_out = dat_all %>% 
  arrange(days_from_start) %>% 
  select(node, days_from_start)

# southern lakes
dat_S = dat %>% 
  filter(metric %in% metrics_want & lakeid %in% c("FI", "ME", "MO", "WI")) %>% 
  mutate(sampledate_fill = as.Date(paste0((year-1), "-12-31")) + daynum_fill)
min_date_S = min(dat_S$sampledate_fill)
dat_S = dat_S %>% 
  mutate(days_from_start = as.numeric(sampledate_fill - min_date_S)) %>% 
  left_join(dictionary_S)
dat_S_out = dat_S %>% 
  arrange(days_from_start) %>% 
  select(node, days_from_start)

# northern lakes
dat_N = dat %>% 
  filter(metric %in% metrics_want & !(lakeid %in% c("FI", "ME", "MO", "WI"))) %>% 
  mutate(sampledate_fill = as.Date(paste0((year-1), "-12-31")) + daynum_fill)
min_date_N = min(dat_N$sampledate_fill)
dat_N = dat_N %>% 
  mutate(days_from_start = as.numeric(sampledate_fill - min_date_N)) %>% 
  left_join(dictionary_N)
dat_N_out = dat_N %>% 
  arrange(days_from_start) %>% 
  select(node, days_from_start)

### write files
# write_csv(dictionary_all, "Data/derived/netcon_dictionary_all.csv")
# write_csv(dictionary_S, "Data/derived/netcon_dictionary_S.csv")
# write_csv(dictionary_N, "Data/derived/netcon_dictionary_N.csv")
# 
# write_csv(dat_all_out, "Data/derived/netcon_data_all.csv")
# write_csv(dat_S_out, "Data/derived/netcon_data_S.csv")
# write_csv(dat_N_out, "Data/derived/netcon_data_N.csv")

# loop thru and do each lake individually
lakes = sort(unique(dat_all$lakeid))
for(i in 1:length(lakes)){
  dat_lake = dat %>% 
    filter(lakeid == lakes[i] & metric %in% metrics_want)%>% 
    mutate(sampledate_fill = as.Date(paste0((year-1), "-12-31")) + daynum_fill) 
  # get minimum date
  min_date_lake = min(dat_lake$sampledate_fill)
  # calc days from start
  dat_lake = dat_lake%>% 
    mutate(days_from_start = as.numeric(sampledate_fill - min_date_lake))
  # generate dictionary
  dictionary_lake = dat_lake %>% 
    select(lakeid, metric) %>% 
    distinct() %>% 
    mutate(node = row_number())
  
  dat_lake_out = dat_lake %>% 
    arrange(days_from_start) %>% 
    left_join(dictionary_lake) %>% 
    select(node, days_from_start)
  
  write_csv(dat_lake_out, paste0("Data/derived/individual_netcon_data/", lakes[i], "_data.csv"))
  write_csv(dictionary_lake, paste0("Data/derived/individual_netcon_data/", lakes[i], "_dictionary.csv"))
}

