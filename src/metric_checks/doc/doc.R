library(tidyverse)
library(lubridate)

nuts0 = read_csv('../../../Data/ntl1_v9_1.csv') # contains all lakes

nuts = nuts0 %>% 
  filter(!is.na(doc)) %>% 
  select(lakeid, year4, daynum, sampledate, depth, rep, sta, event, doc, flagdoc) %>% 
  mutate(sampledate = mdy(sampledate))

nuts %>% 
  ggplot(aes(x=sampledate, y=depth, color=is.na(doc))) +
  geom_point() +
  facet_wrap(~lakeid) +
  scale_y_reverse() # looks like all have 0

nuts %>% 
  group_by(lakeid, depth) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  group_by(lakeid) %>% 
  slice_max(N) # some have slightly more at 4m than 0m; but going with 0 for consistency

# get just surface samples and calculate yearly peak
nuts = nuts %>% 
  filter(depth == 0)
peaks = nuts %>% 
  group_by(lakeid, year4) %>% 
  slice_max(doc)

# look at flags
table(nuts$flagdoc)
# https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-ntl.1.52
def_toss = c()
maybe_toss = c()

lakes = c("FI", "ME", "MO", "WI", "AL", "BM", "CB", "CR", "SP", "TB", "TR")
pdf("../../../Figures/doc_timeseries_withPeaks.pdf", width=11, height=8.5)
for(i in 1:length(lakes)){
  p = nuts %>% 
    filter(lakeid == lakes[i]) %>% 
    ggplot(aes(daynum, doc)) +
    geom_line() +
    geom_vline(data = peaks %>% filter(lakeid == lakes[i]), 
               aes(xintercept=daynum), size=1) +
    facet_wrap(~year4, scales="free_y") +
    theme_bw() +
    ggtitle(paste(lakes[i], "DOC", sep=" - ")) +
    theme(legend.position = c(0.8, 0.08), legend.direction = "horizontal") +
    geom_point(aes(color=flagdoc), size=3)
  print(p)
}
dev.off()

# values to toss:
# FI: H, L
# ME: K, H, J
# MO: H, L 
# WI: H
# AL: D
# BM: D
# CB: D
# CR: D
# SP: D
# TB: AL, D
# TR: D

# set bad values to NA and calculate mean across replicates
nuts_clean = nuts %>% 
  mutate(doc = ifelse(
    (!is.na(flagdoc) & flagdoc  == "D") & (lakeid %in% c("AL", "BM", "CB", "CR", "SP", "TB", "TR")),
    NA,
    doc)) %>% 
  mutate(doc = ifelse(
    (!is.na(flagdoc) & flagdoc %in% c("H", "L")) & (lakeid  %in% c("FI", "MO")),
    NA,
    doc)) %>% 
  mutate(doc = ifelse(
    (!is.na(flagdoc) & flagdoc %in% c("K", "H", "J")) & (lakeid == "ME"),
    NA,
    doc)) %>% 
  mutate(doc = ifelse(
    (!is.na(flagdoc) & flagdoc == "H") & (lakeid == "WI"),
    NA,
    doc)) %>% 
  mutate(doc = ifelse(
    (!is.na(flagdoc) & flagdoc == "AL") & (lakeid == "TB"),
    NA,
    doc))

nuts_clean_avg = nuts_clean %>% 
  group_by(lakeid, year4, daynum, sampledate) %>% 
  summarise(doc = mean(doc, na.rm=T))

peaks_ag = nuts_clean_avg %>% 
  group_by(lakeid, year4) %>% 
  slice_max(doc)

lakes = c("FI", "ME", "MO", "WI", "AL", "BM", "CB", "CR", "SP", "TB", "TR")
pdf("../../../Figures/doc_timeseries_withPeaks_selectFlagsRemoved.pdf", width=11, height=8.5)
for(i in 1:length(lakes)){
  p = nuts_clean_avg %>% 
    filter(lakeid == lakes[i]) %>% 
    ggplot(aes(daynum, doc)) +
    geom_line() +
    geom_vline(data = peaks_ag %>% filter(lakeid == lakes[i]), 
               aes(xintercept=daynum), size=1) +
    facet_wrap(~year4, scales="free_y") +
    theme_bw() +
    ggtitle(paste(lakes[i], "DOC", sep=" - ")) +
    theme(legend.position = c(0.8, 0.08), legend.direction = "horizontal") +
    geom_point()
  print(p)
}
dev.off()

# looks pretty good; a couple outliers but not too bad
# need to remove a few years with just one sampling date
nSamples = nuts_clean_avg %>% 
  group_by(lakeid, year4) %>% 
  summarise(N = n())

good_years = nSamples %>% 
  filter(N > 1) %>% 
  select(-N)

out = left_join(good_years, peaks_ag)
out = out %>% 
  mutate(metric = "doc") %>% 
  select(lakeid, metric, sampledate, year4, daynum) %>% 
  rename(year=year4)

write_csv(out,  "../../../Data/final_metric_data/doc.csv")
                