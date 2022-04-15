library(tidyverse)
library(lubridate)

# read in data
chlN = read_csv( "Data/raw/ntl34_v4_1.csv") # "data1"
chlS0 = read_csv( "Data/raw/ntl38_v5.csv") # "data2"

# format
chlN$sampledate = mdy(chlN$sampledate)
chlN$daynum = yday(chlN$sampledate)
chlS0$sampledate = mdy(chlS0$sampledate)
chlS0$daynum = yday(chlS0$sampledate)

# see where all are NA in the southern lakes
chlS0 %>% 
  filter(is.na(tri_chl_spec) & is.na(mono_chl_spec) & 
           is.na(uncorrect_chl_fluor) & is.na(correct_chl_fluor)) %>% 
  group_by(lakeid) %>% 
  summarise(start_NA = min(sampledate), end_NA = max(sampledate))

chlS = chlS0 %>% 
  select(lakeid, year4, sampledate, depth_range_m, rep, correct_chl_fluor, uncorrect_chl_fluor, tri_chl_spec, flag_fluor, flag_spec)

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
#  -> good to use correct_chl_fluor before and after that; before ~ 1999 use tri_chl_spec to get a few extra years

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

chlS %>% 
  filter(depth_range_m == "0-2") %>% 
  select(flag_spec) %>% 
  table(useNA = 'ifany')
# good to use F, G, FG, H, J; toss JK (depth uncertaint)
chlS = chlS %>% 
  mutate(tri_chl_spec = ifelse(!is.na(flag_spec) & flag_spec == "JK", NA, tri_chl_spec))

chlN %>% 
  filter(depth < 0.5) %>% 
  select(flagchlor) %>% 
  table() # shoot, lots of options
# https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-ntl.35.30
# toss: A, AL (sample suspect); B, BG (standard curve suspect); JK (data suspect); K, KL, KLO, KV, LO, LV, O, OJ, OL, V (suspect),
# keep: D (sample lost, will be NA), G, GJ (analyzed late); GL, JL, L, LG (late/non standard, dup diff but will average), J (non-standard)
chlS_want = chlS %>% 
  filter(depth_range_m == "0-2")

bad_flags = c("A", "AL", "B", "BG", "I", "JK", "K", "KL", "KLO", "KV", "LO", "LV", "O", "OJ", "OL", "V")
chlN %>% 
  filter(depth < 0.5 & !(flagchlor %in% bad_flags)) %>% 
  select(flagchlor) %>% 
  table() # looks good, use these

chlN_want = chlN %>% 
  filter(depth < 0.5 & !(flagchlor %in% bad_flags) & lakeid != "ME")

# calc mean of reps on same date
nReps_S = chlS_want %>% 
  group_by(lakeid, sampledate) %>% 
  summarise(Nreps = n())
table(nReps_S$Nreps)
nReps_S %>% 
  ggplot(aes(sampledate, Nreps)) +
  geom_point() +
  theme_bw() + 
  facet_wrap(~lakeid) #almost always 2 reps

nReps_N = chlN_want %>% 
  group_by(lakeid, sampledate) %>% 
  summarise(Nreps = n())
table(nReps_N$Nreps)
nReps_N %>% 
  ggplot(aes(sampledate, Nreps)) +
  geom_point() +
  theme_bw() + 
  facet_wrap(~lakeid)

chlS_means = chlS_want %>% 
  group_by(lakeid, year4, sampledate) %>% 
  summarise(across(c(correct_chl_fluor, uncorrect_chl_fluor, tri_chl_spec), mean, na.rm=T)) %>% 
  ungroup()

# create column for "best" chl and use that
chlS_means_comb = chlS_means %>% 
  mutate(chl_use = ifelse(!is.na(correct_chl_fluor), correct_chl_fluor, NA),
         chl_use_src = ifelse(!is.na(correct_chl_fluor), "CF", NA)) %>% 
  mutate(chl_use = ifelse(is.na(chl_use) & !is.na(uncorrect_chl_fluor), uncorrect_chl_fluor, chl_use),
         chl_use_src = ifelse(is.na(chl_use_src) & !is.na(chl_use), "UF", chl_use_src)) %>% 
  mutate(chl_use = ifelse(is.na(chl_use) & !is.na(tri_chl_spec), tri_chl_spec, chl_use),
         chl_use_src = ifelse(is.na(chl_use_src) & !is.na(chl_use), "TS", chl_use_src))

chlS_means_comb %>% 
  ggplot(aes(x=sampledate, y=chl_use, color=chl_use_src)) +
  geom_point() +
  facet_wrap(~lakeid) +
  theme_bw()

# check each lake-year for consistency
chlS_means_comb %>% 
  mutate(daynum = lubridate::yday(sampledate)) %>% 
  filter(lakeid == "ME") %>% 
  ggplot(aes(x=daynum, y=chl_use, color=chl_use_src)) +
  geom_point() +
  geom_line()+
  facet_wrap(~year4, scales="free_y") +
  theme_bw()
# most lake-years only have one chl method; those that have multiple look comparable (e.g. 1999)

chlN_means = chlN_want %>% 
  group_by(lakeid, year4, sampledate) %>% 
  summarise(chlor = mean(chlor, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(chlor))

# combine N and S
chl_comb = bind_rows(
  chlS_means_comb %>% rename(chlor = chl_use),
  chlN_means
)

chl_comb$daynum = lubridate::yday(chl_comb$sampledate)

# plot time series
lakes = c("FI", "ME", "MO", "WI", "AL", "BM", "CB", "CR", "SP", "TB", "TR")

pdf("Figures/data_checks/surface_chlorophyll_timeseries.pdf", width=11, height=8.5)
for(i in 1:length(lakes)){
p = chl_comb %>% 
  filter(lakeid == lakes[i]) %>% 
  ggplot(aes(daynum, chlor)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year4, scales="free_y") +
  theme_bw() +
  ggtitle(paste(lakes[i], "Chlorophyll", sep=" - "))
print(p)
}
dev.off()

# ID peaks
daynum_max_all = chl_comb %>% 
  group_by(lakeid, year4) %>% 
  slice_max(chlor) %>% 
  ungroup() %>% 
  mutate(lakeid = factor(lakeid, levels = lakes, ordered=TRUE), period="all")

daynum_max_all %>% 
  ggplot(aes(x=daynum, fill=lakeid)) + 
  geom_histogram() + 
  facet_wrap(~lakeid, ncol=1, strip.position = "right") +
  theme_bw()
# lakes where daynum 200 might not be "natural" split: FI, ME, WI, BM, TB
# looking at pdf created above:
# FI: no clear spring/fall dyanmics; 200 is okay 190ish might be better
# ME: often first spring measurement is largest val, sometime mid/late summer around 200; maybe 175?
# WI: a couple close to 200; 190 or 170 is good
# BM: often random big values in spring; 200 should work well or 175/190
# TB: sometime ridiculously high: (~1000!?!? - check this); otherwise really noisy; often late fall is peak; 175/190/200 probably close to same

daynum_max_spring = chl_comb %>% 
  filter(daynum <= 175) %>% 
  group_by(lakeid, year4) %>% 
  slice_max(chlor) %>% 
  ungroup() %>% 
  mutate(lakeid = factor(lakeid, levels = lakes, ordered=TRUE), period="spring")


daynum_max_fall = chl_comb %>% 
  filter(daynum > 175) %>% 
  group_by(lakeid, year4) %>% 
  slice_max(chlor) %>% 
  ungroup() %>% 
  mutate(lakeid = factor(lakeid, levels = lakes, ordered=TRUE), period="fall")

daynum_max_comb = bind_rows(daynum_max_all, daynum_max_spring, daynum_max_fall)

daynum_max_comb %>% 
  ggplot(aes(x=daynum, fill=period, color=period)) + 
  geom_histogram(position="identity", alpha=0.3) + 
  facet_wrap(~lakeid, ncol=2, strip.position = "right", scales="free_y") +
  theme_bw() +
  geom_vline(xintercept=175) + 
  ggtitle("Chl Peak Timing") +
  theme(legend.position = c(0.75, 0.08), legend.direction = "horizontal")

daynum_max_comb_out = daynum_max_comb %>% 
  rename(year = year4, metric = period) %>% 
  select(-chlor) %>% 
  mutate(metric = ifelse(metric == "all", "chlor_all", metric)) %>% 
  mutate(metric = ifelse(metric == "fall", "chlor_fall", metric)) %>% 
  mutate(metric = ifelse(metric == "spring", "chlor_spring", metric))%>% 
  select(lakeid, metric, sampledate, year, daynum)

# for few peaks where there are multiple peaks, pick the first one
daynum_max_comb_out_singlePeak = daynum_max_comb_out %>% 
  group_by(lakeid, year, metric) %>% 
  summarise(N = n()) %>% 
  filter(N == 1) %>% 
  select(-N)

daynum_max_comb_out_multPeaks = daynum_max_comb_out %>% 
  group_by(lakeid, year, metric) %>% 
  summarise(N = n()) %>% 
  filter(N > 1) %>% 
  select(-N)

hold_peaks = list()
for(i in 1:nrow(daynum_max_comb_out_multPeaks)){
  cur_ly = daynum_max_comb_out %>% 
    filter(lakeid == daynum_max_comb_out_multPeaks$lakeid[i] &
             year == daynum_max_comb_out_multPeaks$year[i] &
             metric == daynum_max_comb_out_multPeaks$metric[i]) %>% 
    arrange(daynum)
  N = ceiling(nrow(cur_ly) / 2)
  hold_peaks[[i]] = cur_ly[N,]
}
all_multpeak_LYs = bind_rows(hold_peaks)

out_chlPeaks_allDOYs = bind_rows(
  daynum_max_comb_out_singlePeak %>% left_join(daynum_max_comb_out), 
  all_multpeak_LYs
) %>% 
  arrange(lakeid, year, metric) %>% 
  filter(!(lakeid %in% c("ME", "MO", "WI", "FI") & year == 2002))

out_chlPeaks_allDOYs %>% 
  ggplot(aes(x=year, y=daynum)) +
  geom_line() +
  geom_point() +
  facet_grid(rows=vars(lakeid), cols=vars(metric))

# write_csv(out_chlPeaks_allDOYs, "Data/final_metric_files/chlorophyll_maxes.csv")

# plot time series with peaks
daynum_max_comb2 = daynum_max_comb %>% 
  mutate(daynum = ifelse(period == "all", daynum + 5, daynum))

pdf("Figures/data_checks/surface_chlorophyll_timeseries_withPeaks.pdf", width=11, height=8.5)
for(i in 1:length(lakes)){
  hold = daynum_max_comb %>% 
    filter(lakeid == lakes[i]) %>% 
    pivot_wider(names_from = "period", values_from = "daynum")
  cur_val = round(sum(hold$all == hold$spring, na.rm=T) / sum(!is.na(hold$all)), 3) * 100
  cur_lab = paste0("Yearly peak in Spring = ", cur_val, "%; Fall = ", 100-cur_val, "%")
                  
  p = chl_comb %>% 
    filter(lakeid == lakes[i]) %>% 
    ggplot(aes(daynum, chlor)) +
    geom_point() +
    geom_line() +
    geom_vline(data = daynum_max_comb2 %>% filter(lakeid == lakes[i]), 
               aes(xintercept=daynum, color = period), size=1) +
    facet_wrap(~year4, scales="free_y") +
    theme_bw() +
    ggtitle(paste(lakes[i], "Chlorophyll", cur_lab, sep=" - ")) +
    theme(legend.position = c(0.8, 0.08), legend.direction = "horizontal")+
    labs(color="Peak Period")
  print(p)
}
dev.off()

# try pracma::findpeaks
for(i in 1:length(lakes)){
  cur_years = chl_comb %>% 
    filter(lakeid == lakes[i]) %>% 
    pull(year4) %>% 
    unique()
  for(y in 1:length(cur_years)){
  cur_ly = chl_comb %>% 
    filter(lakeid == lakes[i] & year4 == cur_years[y]) %>% 
    arrange(daynum)
    if(nrow(cur_ly) > 1){
      hold = pracma::findpeaks(cur_ly %>% pull(chlor))
      if(!is.null(hold)){
        peaks = data.frame(lakeid = lakes[i],
                           year4 = cur_years[y],
                           chl_val = hold[,1], daynum = cur_ly[hold[,2], "daynum"])
        if(i == 1 & y == 1){
          all_peaks = peaks
        }else{
          all_peaks = bind_rows(all_peaks, peaks)
        }
      }
    }
  }
}

# plot pracma:: find peaks
# pdf("Figures/data_checks/surface_chlorophyll_timeseries_withPeaks_pracma.pdf", width=11, height=8.5)
for(i in 1:length(lakes)){
  p = chl_comb %>% 
    filter(lakeid == lakes[i]) %>% 
    ggplot(aes(daynum, chlor)) +
    geom_point() +
    geom_line() +
    geom_vline(data = daynum_max_comb2 %>% filter(lakeid == lakes[i]), 
               aes(xintercept=daynum, color = period), size=1) +
    geom_point(data = all_peaks %>% filter(lakeid == lakes[i]),
               aes(x=daynum, y=chl_val), 
               shape=9, color="orange", size=4) + 
    facet_wrap(~year4, scales="free_y") +
    theme_bw() +
    ggtitle(paste(lakes[i], "Chlorophyll", "(diamonds = pracma ID'd peaks)", sep=" - ")) +
    theme(legend.position = c(0.8, 0.08), legend.direction = "horizontal")+
    labs(color="Peak Period")
  print(p)
}
# dev.off()
# picks up lots of smaller/additonal peak
# unsure if anything helpful there; maybe that it doesn't select first /last pionts as peak? Could also do that above by just dropping first and last observation if needed/wanted?

# Look at "bad" spec values in early 2000s
me_data = read_csv("Data/raw/chlor_ME_95-09_fromLTERserver.csv")
me_data$SAMPLEDATE = mdy(me_data$SAMPLEDATE)
me_data$daynum = yday(me_data$SAMPLEDATE)

me_data %>% 
  # filter(YEAR4 %in% 21:2007) %>% 
  pivot_longer(cols = c("TRI_CHL_SPEC", "CORRECT_CHL_FLUOR")) %>% 
  ggplot(aes(x=SAMPLEDATE, y=value)) +
  facet_wrap(~name, scales="free_y", nrow=2)+
  geom_line() +
  theme_bw()

me_data %>% 
  # filter(YEAR4 %in% 1999:2005) %>% 
  ggplot(aes(x=daynum, y=TRI_CHL_SPEC)) +
  geom_line() +
  theme_bw() + 
  facet_wrap(~YEAR4)

me_data %>% 
  # filter(YEAR4 %in% 1999:2005) %>% 
  ggplot(aes(x=daynum, y=TRI_CHL_SPEC)) +
  geom_line() +
  theme_bw() + 
  facet_wrap(~YEAR4, scales="free_y")

