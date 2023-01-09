library(lubridate)
library(tidyverse)


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
  mutate(iceOn = if_else(sampledate < firstice, TRUE, FALSE)) |> 
  mutate(iceOn = if_else(is.na(iceOn) & month(sampledate) <= 3, TRUE, iceOn)) |> 
  mutate(iceOn = if_else(is.na(iceOn) & month(sampledate) > 3, FALSE, iceOn))

secchi

# # plot by DOY
# ggplot(data=LTERsecchi,aes(x=daynum, y=secnview,col=as.factor(year4)))+
#   geom_point()+
#   geom_line()+
#   facet_wrap(~lakeid,scales="free")

secchi_peaks_formatted = LTERsecchi %>% 
  group_by(lakeid, year4) %>% 
  slice_max(secnview) %>% 
  mutate(metric = "secchi") %>% 
  select(lakeid, metric, sampledate, year4, daynum, secnview)


lakes = c("FI", "ME", "MO", "WI", "AL", "BM", "CB", "CR", "SP", "TB", "TR")
# pdf("Figures/data_checks/secchi_timeseries_withPeaks.pdf", width=11, height=8.5)
# for(i in 1:length(lakes)){
#   p = LTERsecchi %>% 
#     filter(lakeid == lakes[i]) %>% 
#     ggplot(aes(daynum, secnview)) +
#     geom_line() +
#     geom_point()+
#     geom_vline(data = secchi_peaks_formatted %>% filter(lakeid == lakes[i]), 
#                aes(xintercept=daynum), size=1) +
#     facet_wrap(~year4, scales="free_y") +
#     theme_bw() +
#     ggtitle(paste(lakes[i], "Secchi", sep=" - ")) +
#     theme(legend.position = c(0.8, 0.08), legend.direction = "horizontal") 
#   print(p)
# }
# dev.off()

# deal with when there's more than one peak with same max value in a year (see WI) -> take the middle one; round down if even number
secchi_peaks_formatted_singlePeak = secchi_peaks_formatted %>% 
  group_by(lakeid, year4) %>% 
  summarise(N = n()) %>% 
  filter(N == 1) %>% 
  select(-N)

secchi_peaks_formatted_multPeaks = secchi_peaks_formatted %>% 
  group_by(lakeid, year4) %>% 
  summarise(N = n()) %>% 
  filter(N > 1) %>% 
  select(-N)

hold_peaks = list()
for(i in 1:nrow(secchi_peaks_formatted_multPeaks)){
  cur_ly = secchi_peaks_formatted %>% 
    filter(lakeid == secchi_peaks_formatted_multPeaks$lakeid[i] &
             year4 == secchi_peaks_formatted_multPeaks$year4[i]) %>% 
    arrange(daynum)
  N = ceiling(nrow(cur_ly) / 2)
  hold_peaks[[i]] = cur_ly[N,]
}
all_multpeak_LYs = bind_rows(hold_peaks)

out_secchiPeaks_allDOYs = bind_rows(
  secchi_peaks_formatted_singlePeak %>% left_join(secchi_peaks_formatted), 
  all_multpeak_LYs
  ) %>% 
  arrange(lakeid, year4)

# and when there's not that many values? see AL
nsamples = LTERsecchi %>% 
  group_by(lakeid, year4) %>% 
  filter(!is.na(secnview)) %>% 
  summarise(N = n())

sort(nsamples$N)

enough_samples = nsamples %>% 
  filter(N >= 12) %>% 
  select(-N)

out_secchiPeaks_allDOYs = left_join(enough_samples, out_secchiPeaks_allDOYs) %>% 
  mutate(metric = "secchi_all")

#### also, make separate metric for just open water secchi peak? see ME
# get ice on/off dates
ice0 = read_csv("Data/derived/ntl_icedatescombo.csv")
ice0$firstice_year = lubridate::year(ice0$firstice)
ice0$lastice_year = lubridate::year(ice0$lastice)

# not sure how to do this in dplyr; do it in a loop
LTERsecchi$icecovered = NA
ice_nomissing = ice0 %>% 
  filter(!is.na(firstice) & !is.na(lastice))

for(i in 1:nrow(ice_nomissing)){
  cur_lake = ice_nomissing$lakeid[i]
  cur_startdate = ice_nomissing$firstice[i]
  cur_enddate = ice_nomissing$lastice[i]
  inds_covered = LTERsecchi$lakeid == cur_lake & 
    LTERsecchi$sampledate >= cur_startdate &
    LTERsecchi$sampledate <= cur_enddate
  LTERsecchi[inds_covered, "icecovered"] = T
}  

questionable = LTERsecchi %>% 
  filter(!is.na(icecovered) & is.na(ice)) %>% 
  arrange(lakeid, year4) # most of these check out; maybe data entry error?

table(LTERsecchi %>% filter(!is.na(ice)) %>% pull(icecovered), useNA = 'ifany')
table(LTERsecchi %>% filter(!is.na(icecovered)) %>% pull(ice), useNA = 'ifany')

# go with anything with ice or icecovered gets tossed
LTERsecchi_openwater = LTERsecchi %>% 
  filter(is.na(ice) & is.na(icecovered))
table(LTERsecchi_openwater[, c("ice", "icecovered")], useNA = 'ifany')

# repeat checks for enough samples and if there are multiple peaks in a year
secchi_peaks_formatted_ow = LTERsecchi_openwater %>% 
  group_by(lakeid, year4) %>% 
  slice_max(secnview) %>% 
  mutate(metric = "secchi") %>% 
  select(lakeid, metric, sampledate, year4, daynum, secnview) 

# lakes = c("FI", "ME", "MO", "WI", "AL", "BM", "CB", "CR", "SP", "TB", "TR")
# pdf("Figures/data_checks/secchi_timeseries_withPeaks_openwater.pdf", width=11, height=8.5)
# for(i in 1:length(lakes)){
#   p = LTERsecchi_openwater %>% 
#     filter(lakeid == lakes[i]) %>% 
#     ggplot(aes(daynum, secnview)) +
#     geom_line() +
#     geom_point()+
#     geom_vline(data = secchi_peaks_formatted_ow %>% filter(lakeid == lakes[i]), 
#                aes(xintercept=daynum), size=1) +
#     facet_wrap(~year4, scales="free_y") +
#     theme_bw() +
#     ggtitle(paste(lakes[i], "Secchi", sep=" - ")) +
#     theme(legend.position = c(0.8, 0.08), legend.direction = "horizontal") 
#   print(p)
# }
# dev.off() # there are some

secchi_peaks_formatted_singlePeak_ow = secchi_peaks_formatted_ow %>% 
  group_by(lakeid, year4) %>% 
  summarise(N = n()) %>% 
  filter(N == 1) %>% 
  select(-N)

secchi_peaks_formatted_multPeaks_ow = secchi_peaks_formatted_ow %>% 
  group_by(lakeid, year4) %>% 
  summarise(N = n()) %>% 
  filter(N > 1) %>% 
  select(-N)

hold_peaks_ow = list()
for(i in 1:nrow(secchi_peaks_formatted_multPeaks_ow)){
  cur_ly = secchi_peaks_formatted_ow %>% 
    filter(lakeid == secchi_peaks_formatted_multPeaks_ow$lakeid[i] &
             year4 == secchi_peaks_formatted_multPeaks_ow$year4[i]) %>% 
    arrange(daynum)
  N = ceiling(nrow(cur_ly) / 2)
  hold_peaks_ow[[i]] = cur_ly[N,]
}
all_multpeak_LYs_ow = bind_rows(hold_peaks_ow)

out_secchiPeaks_ow = bind_rows(
  secchi_peaks_formatted_singlePeak_ow %>% left_join(secchi_peaks_formatted_ow), 
  all_multpeak_LYs_ow
) %>% 
  arrange(lakeid, year4)

# deal with when there aren't enough samples
nsamples_ow = LTERsecchi_openwater %>% 
  group_by(lakeid, year4) %>% 
  filter(!is.na(secnview)) %>% 
  summarise(N = n())

sort(nsamples_ow$N)

enough_samples_ow = nsamples_ow %>% 
  filter(N >= 10) %>% 
  select(-N)

out_secchiPeaks_ow = left_join(enough_samples_ow, out_secchiPeaks_ow) %>% 
  mutate(metric = "secchi_openwater")

out_all = bind_rows(out_secchiPeaks_allDOYs, out_secchiPeaks_ow) %>% 
  rename(year = year4) %>% 
  select(lakeid, metric, sampledate, year, daynum)

write_csv(out_all, "Data/final_metric_files/secchi.csv")
  