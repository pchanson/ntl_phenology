library(lubridate)
library(tidyverse)
library(scales)

# Updated 2022-12-28
varsWant = c("doc_epiMax", "totpuf_epiMax", "totpuf_epiMin", "totpuf_hypoMax", "totpuf_hypoMin")

#################### LOAD DATA ####################
inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/57/802d63a4c35050b09ef6d1e7da3efd3f"
infile1 <- tempfile()
download.file(inUrl1, infile1, method = "curl")
LTERnutrients <- read_csv(infile1)

# removed flagged data
lternuts.flagged = LTERnutrients %>%
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

# get ice on/off dates
ice0 = read_csv("Data/derived/ntl_icedatescombo.csv") |> 
  mutate(year4 = year(lastice)) |> 
  filter(year4 >= 1980) |> 
  select(-year)

#################### FUNCTIONS ####################
# filtering function - turns outliers into NAs to be removed
filter_lims <- function(x){
  # l <- boxplot.stats(x)$stats[1]
  # u <- boxplot.stats(x)$stats[5]
  l = quantile(x, probs = 0.05)
  u = quantile(x, probs = 0.95)
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
  }
  return(x)
}

#################### MANIPULATE DATA ####################
# Add iceOn status
nutrients = lternuts.flagged |> 
  left_join(ice0 |> select(lakeid, year4, lastice)) |> 
  mutate(iceOn = if_else(sampledate < lastice, TRUE, FALSE)) |> 
  mutate(iceOn = if_else(is.na(iceOn) & month(sampledate) <= 3, TRUE, iceOn)) |> 
  mutate(iceOn = if_else(is.na(iceOn) & month(sampledate) > 3, FALSE, iceOn))

# Load thermocline depth
thermo <- read_csv('Data/derived/thermocline.csv')

# Load stratification dates
strat = read_csv('Data/final_metric_files/physics.csv') %>% 
  filter(metric %in% c('straton', 'stratoff')) %>% 
  select(-daynum) %>% 
  pivot_wider(names_from = "metric", values_from = "sampledate") %>% 
  rename(year4 = year)

# restrict to epilimnion and stratification period
nuts_epi = nutrients %>% left_join(thermo, by = c("lakeid", "sampledate")) %>% 
  left_join(strat, by = c("lakeid", "year4")) %>% 
  filter(depth <= thermdepth_m) %>% #filter to epilimnion
  filter(sampledate >= straton & daynum <= stratoff) %>% #filter to during strat
  filter(year4 > 1981)

nuts_hypo = lternuts.flagged %>% left_join(thermo, by = c("lakeid", "sampledate")) %>% 
  left_join(strat, by = c("lakeid", "year4")) %>% 
  filter(depth > thermdepth_m) %>% #filter to epilimnion
  filter(sampledate >= straton & daynum <= stratoff) %>% #filter to during strat
  filter(year4 > 1981)

# What plot looks like with outliers
# DOC
# ggplot(nuts_epi %>% filter(item == 'doc')) +
#   geom_path(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   geom_point(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   scale_x_date(labels = date_format("%b")) +
#   facet_wrap(~lakeid, scales = 'free_y') +
#   theme_bw(base_size = 9) +
#   theme(axis.title.x = element_blank())
# 
# ggplot(nuts_hypo %>% filter(item == 'doc')) +
#   geom_path(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   geom_point(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   scale_x_date(labels = date_format("%b")) +
#   facet_wrap(~lakeid, scales = 'free_y') +
#   theme_bw(base_size = 9) +
#   theme(axis.title.x = element_blank())
# 
# # TP
# ggplot(nuts_epi %>% filter(item == 'totpuf')) +
#   geom_path(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   geom_point(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   scale_x_date(labels = date_format("%b")) +
#   facet_wrap(~lakeid, scales = 'free_y') +
#   theme_bw(base_size = 9) +
#   theme(axis.title.x = element_blank())
# 
# ggplot(nuts_hypo %>% filter(item == 'totpuf')) +
#   geom_path(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   geom_point(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   scale_x_date(labels = date_format("%b")) +
#   facet_wrap(~lakeid, scales = 'free_y') +
#   theme_bw(base_size = 9) +
#   theme(axis.title.x = element_blank())
# 
# # TN
# ggplot(nuts_epi %>% filter(item == 'totnuf')) +
#   geom_path(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   geom_point(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   scale_x_date(labels = date_format("%b")) +
#   facet_wrap(~lakeid, scales = 'free_y') +
#   theme_bw(base_size = 9) +
#   theme(axis.title.x = element_blank())
# 
# ggplot(nuts_hypo %>% filter(item == 'totnuf')) +
#   geom_path(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   geom_point(aes(x = as.Date(yday(sampledate), origin = as.Date('2019-01-01')), y = value, group = year4)) +
#   scale_x_date(labels = date_format("%b")) +
#   facet_wrap(~lakeid, scales = 'free_y') +
#   theme_bw(base_size = 9) +
#   theme(axis.title.x = element_blank())

# Exclude outliers based on statistics. Remove < 5th and > 95th percentile
nuts_epi2 = nuts_epi %>% 
  group_by(lakeid, item) %>% 
  filter(item %in% c('doc', 'totpuf', 'totnuf')) %>% 
  mutate(value = filter_lims(value)) %>% 
  group_by(lakeid, item, sampledate, year4) %>% 
  summarise(value = mean(value, na.rm = T))

nuts_hypo2 = nuts_hypo %>% 
  group_by(lakeid, item) %>% 
  filter(item %in% c('doc', 'totpuf', 'totnuf')) %>% 
  mutate(value = filter_lims(value)) %>% 
  group_by(lakeid, item, sampledate, year4) %>% 
  summarise(value = mean(value, na.rm = T))

# find day of max DOC
epi_min = nuts_epi2 %>% 
  group_by(lakeid, year4, item) %>% 
  slice_min(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
  mutate(daynum = yday(sampledate), metric = paste0(item, "_epiMin")) %>% 
  ungroup() %>% 
  select(lakeid, metric, sampledate, year4, daynum)

epi_max = nuts_epi2 %>% 
  group_by(lakeid, year4, item) %>% 
  slice_max(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
  mutate(daynum = yday(sampledate), metric = paste0(item, "_epiMax")) %>% 
  ungroup() %>% 
  select(lakeid, metric, sampledate, year4, daynum)

hypo_min = nuts_hypo2 %>% 
  group_by(lakeid, year4, item) %>% 
  slice_min(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
  mutate(daynum = yday(sampledate), metric = paste0(item, "_hypoMin")) %>% 
  ungroup() %>% 
  select(lakeid, metric, sampledate, year4, daynum)

hypo_max = nuts_hypo2 %>% 
  group_by(lakeid, year4, item) %>% 
  slice_max(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
  mutate(daynum = yday(sampledate), metric = paste0(item, "_hypoMax")) %>% 
  ungroup() %>% 
  select(lakeid, metric, sampledate, year4, daynum)

comb = bind_rows(epi_min, epi_max, hypo_min, hypo_max) |> 
  rename(year = year4)

# Plot density distributions
# ggplot(comb) +
#   geom_density(aes(x = daynum)) +
#   facet_grid(rows=vars(lakeid), cols=vars(metric))

write.csv(comb %>% filter(metric %in% varsWant),"Data/final_metric_files/nutrients.csv", row.names = F)
