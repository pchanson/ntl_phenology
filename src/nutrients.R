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

# removed flagged data
lternuts.flagged = read_csv('Data/ntl1_v9_1.csv') %>%
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

# Load thermocline depth
thermo <- read_csv('Data/thermocline.csv') |> 
  rename(lakeid = id)

# Load stratification dates
strat = read_csv('Data/final_metric_data/physics.csv') |> 
  filter(metric == 'straton') |> 
  select(lakeid, year4 =year , stratday = daynum)


# restrict to epilimnion and stratification period
nuts = lternuts.flagged |> left_join(thermo, by = c("lakeid", "sampledate")) |> 
  left_join(strat, by = c("lakeid", "year4")) |> 
  mutate(month = month(sampledate), year = year(sampledate), yday = yday(sampledate)) |> 
  filter(depth <= thermdepth_m) |> #filter to epilimnion
  filter(daynum >= stratday) |> #filter to after stratification
  filter(year > 1981) 

# What plot looks like with outliers
ggplot(nuts |> filter(item == 'doc')) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

# Exclude outliers based on box plot statistics 
nuts2 = nuts |> 
  group_by(lakeid) |> 
  filter(item == 'doc') |> 
  mutate(value = filter_lims(value)) |> 
  group_by(lakeid, sampledate, year, yday) |> 
  summarise(value = mean(value, na.rm = T))

ggplot(nuts2) +
  geom_path(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  geom_point(aes(x = as.Date(yday, origin = as.Date('2019-01-01')), y = value, group = year)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~lakeid, scales = 'free_y') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

# find day of max DOC
doc = nuts2 |> group_by(lakeid, year) |> 
  filter(value == max(value, na.rm = T)) |> 
  mutate(daynum = yday(sampledate)) |> 
  select(lakeid, year4 = year, daynum = yday)

# Plot density distributions
ggplot(doc) +
  geom_density(aes(x = daynum)) +
  facet_wrap(~lakeid)

write.csv(doc,"Data/doc.csv")
