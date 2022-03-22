library(NTLlakeloads)
library(tidyverse)

# data<-loadLTERsecchi()

inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/29/5a5a5606737d760b61c43bc59460ccc9"
infile1 <- tempfile()
download.file(inUrl1, infile1, method = "libcurl")
LTERsecchi <- read_csv(infile1, skip = 1, quote = "\"", guess_max = 1e+05, 
                       col_names = c("lakeid", "year4", "daynum", "sampledate", 
                                     "sta", "secview", "secnview", "timeon", "timeoff", 
                                     "airtemp", "windir", "windspd", "waveht", "cloud", 
                                     "ice"))

table(LTERsecchi$lakeid)

# plot by DOY
ggplot(data=LTERsecchi,aes(x=daynum, y=secnview,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

# plot each lake-year separately
secchi_peaks_formatted = LTERsecchi %>% 
  group_by(lakeid, year4) %>% 
  slice_max(secnview) %>% 
  mutate(metric = "secchi") %>% 
  select(lakeid, metric, sampledate, year4, daynum, secnview) 
# %>% 
  # rename(year = year4)


lakes = c("FI", "ME", "MO", "WI", "AL", "BM", "CB", "CR", "SP", "TB", "TR")
pdf("../../../Figures/secchi_timeseries_withPeaks.pdf", width=11, height=8.5)
for(i in 1:length(lakes)){
  p = LTERsecchi %>% 
    filter(lakeid == lakes[i]) %>% 
    ggplot(aes(daynum, secnview)) +
    geom_line() +
    geom_point()+
    geom_vline(data = secchi_peaks_formatted %>% filter(lakeid == lakes[i]), 
               aes(xintercept=daynum), size=1) +
    facet_wrap(~year4, scales="free_y") +
    theme_bw() +
    ggtitle(paste(lakes[i], "Secchi", sep=" - ")) +
    theme(legend.position = c(0.8, 0.08), legend.direction = "horizontal") 
  print(p)
}
dev.off()

# TODO: deal with when there's more than one peak with same max value in a year (see WI)
# and when there's not that many values? see AL