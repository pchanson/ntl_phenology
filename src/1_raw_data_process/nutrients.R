library(lubridate)
library(tidyverse)
library(scales)

# filtering function - turns outliers into NAs to be removed
filter_lims <- function(x){
  l <- boxplot.stats(x)$stats[1]
  u <- boxplot.stats(x)$stats[5]
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
  }
  return(x)
}

varsWant = c("doc_epiMax", "totpuf_epiMax", "totpuf_epiMin", "totpuf_hypoMax", "totpuf_hypoMin")

# removed flagged data
lternuts.flagged = read_csv('Data/raw/ntl1_v9_1.csv') %>%
  mutate(across(everything(), ~replace(., .<0 , NA))) %>%
  rename_all( ~ str_replace(., "_sloh", '.sloh')) %>%
  rename_all( ~ str_replace(., "_n", '.n')) %>%
  rename_at(vars(ph:drsif.sloh), ~ str_c("value_",.)) %>%
  rename_at(vars(flagdepth:flagdrsif.sloh), ~ str_c("error_",.)) %>%
  rename_all(~str_replace_all(.,"flag","")) %>%
  pivot_longer(-(lakeid:event), names_to = c('.value','item'), names_sep = '_') %>%
  filter(!is.na(value) & value>= 0) %>%
  filter(!str_detect(error,'A|K|L|H') | is.na(error)) %>%
  select(-error) %>% 
  mutate(value = case_when(str_detect(item, ".sloh") ~ value*1000, #change sloh from mg to µg
                           TRUE ~ value)) %>% 
  mutate(item = case_when(str_detect(item, ".sloh") ~  str_remove(item, ".sloh"),
                          TRUE ~ item))

# add flag for ice dates
ice0 = read_csv("Data/derived/ntl_icedatescombo.csv")
ice0$firstice_year = lubridate::year(ice0$firstice)
ice0$lastice_year = lubridate::year(ice0$lastice)

lternuts.flagged$icecovered = NA
colsChange = c("lastice", "lasticeYDAY", "lastice_year")
ice0[ice0$lakeid == "CB" & ice0$year == 1988, colsChange] =
  ice0[ice0$lakeid == "AL" & ice0$year == 1988, colsChange]
ice_nomissing = ice0 %>% 
  filter(!is.na(firstice) & !is.na(lastice))
for(i in 1:nrow(ice_nomissing)){
  cur_lake = ice_nomissing$lakeid[i]
  cur_startdate = ice_nomissing$firstice[i]
  cur_enddate = ice_nomissing$lastice[i]
  inds_covered = lternuts.flagged$lakeid == cur_lake & 
    lternuts.flagged$sampledate >= cur_startdate &
    lternuts.flagged$sampledate <= cur_enddate
  if(cur_lake == "MO"){
    inds_covered = inds_covered | (
      lternuts.flagged$lakeid == "FI" & 
        lternuts.flagged$sampledate >= cur_startdate &
        lternuts.flagged$sampledate <= cur_enddate
    )
  }
  
  lternuts.flagged[inds_covered, "icecovered"] = T
}  

# Load thermocline depth
thermo <- read_csv('Data/derived/thermocline.csv')

# Load stratification dates
strat = read_csv('Data/final_metric_files/physics.csv') %>% 
  filter(metric %in% c('straton', 'stratoff')) %>% 
  select(-daynum) %>% 
  pivot_wider(names_from = "metric", values_from = "sampledate") %>% 
  mutate(straton_daynum = lubridate::yday(straton), 
         stratoff_daynum = lubridate::yday(stratoff))%>% 
  select(lakeid, year4 =year , straton_daynum, stratoff_daynum)



# restrict to epilimnion and stratification period
nuts_epi = lternuts.flagged %>% left_join(thermo, by = c("lakeid", "sampledate")) %>% 
  left_join(strat, by = c("lakeid", "year4")) %>% 
  mutate(month = month(sampledate), year = year(sampledate), yday = yday(sampledate)) %>% 
  filter(depth <= thermdepth_m) %>% #filter to epilimnion
  filter(daynum >= straton_daynum & daynum < stratoff_daynum) %>% #filter to during strat
  # filter(is.na(icecovered)) %>% # filter to non-ice dates; gets X% more rows than filtering on strat
  filter(year > 1981)

nuts_hypo = lternuts.flagged %>% left_join(thermo, by = c("lakeid", "sampledate")) %>% 
  left_join(strat, by = c("lakeid", "year4")) %>% 
  mutate(month = month(sampledate), year = year(sampledate), yday = yday(sampledate)) %>% 
  filter(depth > thermdepth_m) %>% #filter to epilimnion
  filter(daynum >= straton_daynum & daynum < stratoff_daynum) %>% #filter to during strat
  # filter(is.na(icecovered)) %>% # filter to non-ice dates; gets Y% more rows than filtering on strat
  filter(year > 1981)

# What plot looks like with outliers
# DOC
ggplot(nuts_epi %>% filter(item == 'doc')) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

ggplot(nuts_hypo %>% filter(item == 'doc')) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

# TP
ggplot(nuts_epi %>% filter(item == 'totpuf')) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

ggplot(nuts_hypo %>% filter(item == 'totpuf')) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

# TN
ggplot(nuts_epi %>% filter(item == 'totnuf')) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

ggplot(nuts_hypo %>% filter(item == 'totnuf')) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

# Exclude outliers based on box plot statistics 
nuts_epi2 = nuts_epi %>% 
  group_by(lakeid, item) %>% 
  filter(item %in% c('doc', 'totpuf', 'totnuf')) %>% 
  mutate(value = filter_lims(value)) %>% 
  group_by(lakeid, item, sampledate, year, yday) %>% 
  summarise(value = mean(value, na.rm = T))

nuts_hypo2 = nuts_hypo %>% 
  group_by(lakeid, item) %>% 
  filter(item %in% c('doc', 'totpuf', 'totnuf')) %>% 
  mutate(value = filter_lims(value)) %>% 
  group_by(lakeid, item, sampledate, year, yday) %>% 
  summarise(value = mean(value, na.rm = T))

ggplot(nuts_epi2 %>% filter(item == 'totpuf')) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank()) +
  ggtitle("Epi TP")

ggplot(nuts_hypo2 %>% filter(item == 'totpuf')) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank()) +
  ggtitle("Hypo TP")

# find day of max DOC
epi_min = nuts_epi2 %>% 
  group_by(lakeid, year, item) %>% 
  filter(value == min(value, na.rm = T)) %>% 
  mutate(daynum = yday(sampledate), metric = paste0(item, "_epiMin")) %>% 
  ungroup() %>% 
  select(lakeid, metric, sampledate, year, daynum = yday)

epi_max = nuts_epi2 %>% 
  group_by(lakeid, year, item) %>% 
  filter(value == max(value, na.rm = T)) %>% 
  mutate(daynum = yday(sampledate), metric = paste0(item, "_epiMax")) %>% 
  ungroup() %>% 
  select(lakeid, metric, sampledate, year, daynum = yday)

hypo_min = nuts_hypo2 %>% 
  group_by(lakeid, year, item) %>% 
  filter(value == min(value, na.rm = T)) %>% 
  mutate(daynum = yday(sampledate), metric = paste0(item, "_hypoMin")) %>% 
  ungroup() %>% 
  select(lakeid, metric, sampledate, year, daynum = yday)

hypo_max = nuts_hypo2 %>% 
  group_by(lakeid, year, item) %>% 
  filter(value == max(value, na.rm = T)) %>% 
  mutate(daynum = yday(sampledate), metric = paste0(item, "_hypoMax")) %>% 
  ungroup() %>% 
  select(lakeid, metric, sampledate, year, daynum = yday)

comb = bind_rows(epi_min, epi_max, hypo_min, hypo_max)

# see if any years have multiple peaks with same doc value
comb %>% group_by(lakeid, year, metric) %>% summarise(N = n()) %>% filter(N > 1) %>% View()
# Sparkling 1997

nuts_epi2 %>% filter(lakeid == "TR" & year == 1996) %>% View() # two or three dates with same value take first one as done w/ other values
# TODONE: limit this to either after strat or ice-off, then re-calculate and write; used strat so that epi/hypo were meaningful
comb = comb %>% 
  group_by(lakeid, year, metric) %>% 
  slice_min(daynum) %>% 
  ungroup()

# Plot density distributions
ggplot(comb) +
  geom_density(aes(x = daynum)) +
  facet_grid(rows=vars(lakeid), cols=vars(metric))

write.csv(comb %>% filter(metric %in% varsWant),"Data/final_metric_files/nutrients.csv", row.names = F)
