library(tidyverse)
library(lubridate)

dat0 = read_csv("Data/raw/chl_from_server_files_v2.csv") %>% 
  rename(lakeid = Lake, sampledate = `Sample Date`, depth_range_m = `Depth (m)`,
         correct_chl_fluor = `corrected chloro a`, uncorrect_chl_fluor = `uncor. chloro a`) %>% 
  mutate(sampledate = mdy(sampledate))

dat = dat0 %>% 
  filter(lakeid %in% c("FI", "ME", "MO", "WI") & depth_range_m == "0-2") %>% 
  group_by(lakeid, sampledate, depth_range_m) %>% 
  summarise(across(correct_chl_fluor:uncorrect_chl_fluor, mean)) %>% 
  mutate(year = year(sampledate), daynum = yday(sampledate))

# some plots to visualize issue
dat %>% 
  ggplot(aes(x=sampledate, y=correct_chl_fluor)) +
  geom_line() +
  geom_point() + 
  facet_wrap(~lakeid) +
  theme_bw()

peaks_uncor = dat %>% 
  group_by(lakeid, year) %>% 
  slice_max(uncorrect_chl_fluor)

peaks_cor = dat %>% 
  group_by(lakeid, year) %>% 
  slice_max(correct_chl_fluor)

dat  %>% 
  group_by(lakeid, year) %>% 
  ggplot() +
  geom_line(aes(x=daynum, y=uncorrect_chl_fluor), color="black") +
  geom_point(aes(x=daynum, y=uncorrect_chl_fluor), color="black") + 
  geom_line(aes(x=daynum, y=correct_chl_fluor), color="red") +
  geom_point(aes(x=daynum, y=correct_chl_fluor), color="red") +
  facet_grid(rows=vars(year), cols=vars(lakeid), scales="free_y") +
  theme_bw() +
  ggtitle("Concentrations, only fluorometer values; black=uncorrected, red=corrected \n Vertical lines = day of peak") +
  theme(legend.position = "none") +
  geom_vline(data = peaks_uncor, mapping=aes(xintercept=daynum), color="black")+
  geom_vline(data = peaks_cor, mapping=aes(xintercept=daynum), color="red")

dat  %>% 
  group_by(lakeid, year) %>% 
  mutate(uncor_chl_fluor_scaled = scale(uncorrect_chl_fluor),
         cor_chl_fluor_scaled = scale(correct_chl_fluor)) %>% 
  ggplot()+
  geom_line(aes(x=daynum, y=uncor_chl_fluor_scaled), color="black") +
  geom_point(aes(x=daynum, y=uncor_chl_fluor_scaled), color="black") + 
  geom_line(aes(x=daynum, y=cor_chl_fluor_scaled), color="red") +
  geom_point(aes(x=daynum, y=cor_chl_fluor_scaled), color="red") +
  facet_grid(rows=vars(year), cols=vars(lakeid), scales="free_y") +
  theme_bw() +
  ggtitle("Indep. lake-year z-scores; only fluorometer values; red=corrected_fluor, black=uncorr_fluor") +
  theme(legend.position = "none") +
  geom_vline(data = peaks_uncor, mapping=aes(xintercept=daynum), color="black")+
  geom_vline(data = peaks_cor, mapping=aes(xintercept=daynum), color="red")

# compare to spec data from bad years
me = read_csv("Data/raw/chlor_ME_95-09_fromLTERserver.csv") %>% 
  select(lakeid = LAKEID, year = YEAR4, sampledate = SAMPLEDATE, 
         depth_range_m = DEPTH_RANGE_M, rep = REP, tri_chl_spec = TRI_CHL_SPEC) %>% 
  mutate(sampledate = mdy(sampledate), daynum = yday(sampledate)) %>% 
  group_by(lakeid, year, sampledate, depth_range_m, daynum) %>% 
  summarise(tri_chl_spec = mean(tri_chl_spec))

comb = left_join(me, dat) %>% 
  group_by(lakeid, year) %>% 
  mutate(cor_chl_fluor_scale = scale(correct_chl_fluor),
         uncor_chl_fluor_scale = scale(uncorrect_chl_fluor),
         tri_chl_spec_scale = scale(tri_chl_spec)) %>% 
  filter(year >= 2000)

peaks_spec = comb %>% 
  group_by(lakeid, year) %>% 
  slice_max(tri_chl_spec)

comb %>% 
  ggplot()+
  geom_line(aes(x=daynum, y=uncor_chl_fluor_scale), color="black") +
  geom_point(aes(x=daynum, y=uncor_chl_fluor_scale), color="black") + 
  geom_line(aes(x=daynum, y=cor_chl_fluor_scale), color="red") +
  geom_point(aes(x=daynum, y=cor_chl_fluor_scale), color="red") +
  geom_line(aes(x=daynum, y=tri_chl_spec_scale), color="blue") +
  geom_point(aes(x=daynum, y=tri_chl_spec_scale), color="blue") +
  facet_grid(rows=vars(year), cols=vars(lakeid), scales="free_y") +
  theme_bw() +
  ggtitle("Indep. z-scores by year; blue=tri_spec, red=corrected_fluor, black=uncorr_fluor") +
  geom_vline(data = peaks_uncor %>% filter(lakeid == "ME"), mapping=aes(xintercept=daynum), color="black")+
  geom_vline(data = peaks_cor %>% filter(lakeid == "ME"), mapping=aes(xintercept=daynum), color="red")+
  geom_vline(data = peaks_spec %>% filter(lakeid == "ME"), mapping=aes(xintercept=daynum), color="blue")
  