library(tidyverse)
library(lubridate)

# read in data
chlN = read_csv( "../../../Data/ntl34_v4_1.csv") # "data1"
chlS0 = read_csv( "../../../Data/ntl38_v5.csv") # "data2"

# format
chlN$sampledate = mdy(chlN$sampledate)
chlN$daynum = yday(chlN$sampledate)
chlS0$sampledate = mdy(chlS0$sampledate)
chlS0$daynum = yday(chlS0$sampledate)

chlS = chlS0 %>% 
  select(lakeid, year4, sampledate, depth_range_m, rep, correct_chl_fluor, flag_fluor)

# look at depths
chlN %>% 
  ggplot(aes(x=sampledate, y=depth, color=chlor)) +
  geom_point() +
  facet_wrap(~lakeid, scales="free") + 
  theme_bw() +
  scale_color_viridis_c() # depths in N look pretty consistent through time

table(chlN[, c("lakeid", "depth")]) # lake specific:

chlN %>% 
  ggplot(aes(x=depth)) +
  geom_histogram() +
  facet_wrap(~lakeid, scales="free") + 
  theme_bw() # all have 0m; next consistent depth varies between 1m (CB), 2m (AL, CB, TB), 3m (BM, CR, SP, TR)
# for consistency, just do 0m

chlS %>% 
  ggplot(aes(x=sampledate, y=depth_range_m, color=correct_chl_fluor)) +
  geom_point() +
  facet_wrap(~lakeid, scales="free") + 
  theme_bw() +
  scale_color_viridis_c() # 0-2 most consistent

#look for non-NA values in other chl measures
chlS0 %>% 
  filter(depth_range_m == "0-2") %>% 
  select(-c("rep", starts_with(c("phaeo", "flag")))) %>% 
  pivot_longer(cols = all_of(c("tri_chl_spec", "mono_chl_spec", "uncorrect_chl_fluor", "correct_chl_fluor"))) %>% 
  filter(!is.na(value)) %>% 
  ggplot() +
  geom_point(aes(x=sampledate, y=name, color=!is.na(value))) +
  facet_wrap(~lakeid, scales="free") + 
  theme_bw() + 
  labs(color="has value") # looks like all values from ~ summer 2002 to 2005 are NA, extends even longer for spec 
#  -> good to use correct_chl_fluor

# look at flags
chlS %>% 
  filter(depth_range_m == "0-2") %>% 
  select(flag_fluor) %>% 
  table(useNA = 'ifany') # C, D, NA
# metadata: https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-ntl.38.27
# C = fluorometer issue and apparently only uncorrected_chl values reported?
chlS0 %>% 
  filter(depth_range_m == "0-2" & flag_fluor == "C") # can probably use these values; but need to merge them; only 1999 -> actually, just a few points at beginning, at most get you another year; good to ignore
# D = new fluorometer with correct settings / filters; good to use

chlN %>% 
  filter(depth < 0.5) %>% 
  select(flagchlor) %>% 
  table() # damn, lots of options
# toss: A, AL (sample suspect); B, BG (standard curve suspect); JK (data suspect); K, KL, KLO, KV, LO, LV, O, OJ, OL, V (suspect),
# keep: D (sample lost, will be NA), G, GJ (analyzed late); GL, JL, L, LG (late/non standard, dup diff but will average), 


# calc mean of reps on same date
nReps 

# plot time series

# ID peaks

# plot time series with peaks


