# remove everything from workspace
rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Package ID: knb-lter-ntl.29.29 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.

inUrl3 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/29/03e232a1b362900e0f059859abe8eb97"
infile3 <- tempfile()
download.file(inUrl3, infile3, method = "curl")
dt1 <- read_csv(infile3, skip = 1, quote = "\"", guess_max = 1e+05, 
                     col_names = c("lakeid", "year4", "daynum", "sampledate", 
                                   "depth", "rep", "sta", "event", "wtemp", "o2", "o2sat", 
                                   "deck", "light", "frlight", "flagdepth", "flagwtemp", 
                                   "flago2", "flago2sat", "flagdeck", "flaglight", "flagfrlight"))
dt1
   

# packages
# install.packages('devtools')
# devtools::install_github('hdugan/NTLlakeloads')
# install.packages('tidyr')
# install.packages('tidyverse')
# install.packages('rLakeAnalyzer')
# install.packages('lubridate')
# install.packages('zoo')
# devtools::install_github('thomasp85/gganimate')
# install.packages('ggridges')

library(NTLlakeloads)
library(tidyverse)
library(rLakeAnalyzer)
library(lubridate)
library(zoo)
library(patchwork)
library(gganimate)
library(ggridges)
library(pracma)
library(scales)

get_dens <- function(temp, salt){
  dens = 999.842594 + (6.793952 * 10^-2 * temp) - (9.095290 * 10^-3 * temp^2) +
    (1.001685 * 10^-4 * temp^3) - (1.120083 * 10^-6 * temp^4) + (6.536336 * 10^-9 * temp^5) +
    (8.24493 * 10^-1 -4.0899 * 10^-3 * temp+ 7.6438 * 10^-5 * temp^2 - 8.2467 * 10^-7 * temp^3 + 
       5.3875 * 10^-9* temp^4) * salt+
    (-5.72466 *  10^-3 + 1.0227 * 10^-4 * temp -1.6546 * 10^-6 * temp^2) * salt^(3/2) +
    (4.8314*  10^-4 ) * salt
  return(dens)
}

bath <- read.csv('../Data/NTLhypsometry.csv')
bath <- rbind(bath, data.frame('lakeid' = rep('FI',2), 'Depth_m' = c(0,18.9),
                               'Depth_ft' = c(0,0),'area' = c(874000, 0)))

ntl.id <- unique(dt1$lakeid)
strat.df <- data.frame('year' = NULL, 'straton' = NULL, 'stratoff' = NULL, 'duration' = NULL,
                       'energy' = NULL, 'stability' = NULL, 'anoxia' = NULL, 'id' = NULL)
en.df <- data.frame('sampledate' = NULL, 'energy' = NULL, 'n2' = NULL, 'id' = NULL)

therm.df <- data.frame('sampledate' = NULL, 'thermdepth_m' = NULL, 'id' = NULL)

for (name in ntl.id){
  print(name)
  
  data <- dt1 %>%
    filter(lakeid == name) %>%
    group_by(sampledate) %>%
    filter(is.na(flagwtemp)) %>%
    filter(!is.na(wtemp)) %>%
    filter(sum(!is.na(wtemp))>1) %>%
    fill(wtemp, .direction = 'up') %>%
    fill(wtemp, .direction = 'down') %>%
    # mutate(wtemp = ifelse(row.number() ==1 & is.na(wtemp), lead(wtemp), wtemp)) %>%
    # mutate(ifelse(is.na(wtemp[which.min(depth)])), wtemp[which.min(depth+1)], wtemp[which.min(depth)]) %>%
    mutate(iwtemp = na.approx(wtemp)) %>%
    mutate(wdens = get_dens(iwtemp, 0)) %>%
    select(year4, sampledate, depth, iwtemp, wtemp, wdens)
  
  data.o2 <- dt1 %>%
    filter(lakeid == name) %>%
    group_by(sampledate) %>%
    filter(is.na(flagwtemp)) %>%
    filter(!is.na(o2)) %>%
    filter(sum(!is.na(o2))>1) %>%
    fill(o2, .direction = 'up') %>%
    fill(o2, .direction = 'down') %>%
    # mutate(wtemp = ifelse(row.number() ==1 & is.na(wtemp), lead(wtemp), wtemp)) %>%
    # mutate(ifelse(is.na(wtemp[which.min(depth)])), wtemp[which.min(depth+1)], wtemp[which.min(depth)]) %>%
    mutate(io2 = na.approx(o2)) %>%
    select(year4, sampledate, depth, io2, o2)
  
  
  for (a in unique(data$year4)){
    
    hyp <- bath %>%
      filter(lakeid == name)
    if (max(data$depth) > max(hyp$Depth_m)){
      hyp <- rbind(hyp, hyp[nrow(hyp),])
      hyp$Depth_m[nrow(hyp)] <- max(data$depth)
    }
    
    df <- data %>%
      filter(year4 == a) %>%
      group_by(sampledate) %>%
      distinct(depth, .keep_all = TRUE) %>%
      # arrange(depth) %>%
      mutate(dup = duplicated(depth)) %>%
      summarise(#metadeps = meta.depths(wtr = iwtemp[which(dup == FALSE)], 
                 #                      depths = depth[which(dup == FALSE)], slope = 0.1, seasonal = TRUE, mixed.cutoff = 1),
                thermdep = thermo.depth(wtr = iwtemp[which(dup == FALSE)], depths = depth[which(dup == FALSE)], 
                                        Smin = 0.1, seasonal = TRUE, index = FALSE,
                                        mixed.cutoff = 1),
                densdiff = wdens[which.max(depth)] - wdens[which.min(depth)],
                surfwtemp = iwtemp[which.min(depth)]) 
    
    therm.df <- rbind(therm.df, data.frame('sampledate' = df$sampledate, 'thermdepth_m' = df$thermdep, 'id' = name))
    
    border <- floor(mean(df$thermdep, na.rm = T))
    
    dz = 0.1
    en <- data %>%
      filter(year4 == a) %>%
      group_by(sampledate) %>%
      arrange(depth) %>%
      summarise(z = seq(min(depth),max(depth),dz),
                area = approx(hyp$Depth_m, hyp$area, seq(min(depth), max(depth),dz))$y,
                density = approx(depth, wdens, seq(min(depth), max(depth),dz))$y,
                temp = approx(depth, wtemp, seq(min(depth), max(depth),dz))$y) %>%
      mutate('energy' = (area * dz) * density *temp * 4186,
             'n2' = c(0,buoyancy.freq(temp, z))) %>%
      summarise('energy' = sum(energy, na.rm = T)/max(area, na.rm = T),
                'n2max' = max(n2))
    
    
    df = df %>% mutate(densdiff = ifelse(densdiff > 0.1 & surfwtemp >= 4, densdiff, NA))
    
    df <- df[complete.cases(df),]
    
    an <- data.o2 %>%
      filter(year4 == a) %>%
      group_by(sampledate) %>%
      arrange(depth) %>%
      summarise(z = seq(min(depth),max(depth),dz),
                area = approx(hyp$Depth_m, hyp$area, seq(min(depth), max(depth),dz))$y,
                do = approx(depth, o2, seq(min(depth), max(depth), dz))$y) %>%
      filter(z >= border & !is.na(do)) %>%
      # filter(!is.na(do) & sampledate > (df$sampledate[which.min(df$sampledate)])) %>%
      summarise('do' = abs(trapz(z * area, do)))

    strat.df <- rbind(strat.df, data.frame('year' = a,
                                           'straton' = yday(df$sampledate[which.min(df$sampledate)]),
                                           'stratoff' = yday(df$sampledate[which.max(df$sampledate)]),
                                           'duration' = yday(df$sampledate[which.max(df$sampledate)]) - yday(df$sampledate[which.min(df$sampledate)]),
                                           'energy' = yday(en$sampledate[which.max(en$energy)]),
                                           'stability' = yday(en$sampledate[which.max(en$n2max)]),
                                           'anoxia' = yday(an$sampledate[which.min(an$do)]),
                                           'id' = name))
    en.df <- rbind(en.df, data.frame('sampledate' = en$sampledate, 'energy' = en$energy, 'n2' = en$n2max,
                                     id = rep(name, nrow(en))))
  }
  
}

str(therm.df)
write.csv(therm.df, file ='../Data/thermocline.csv', quote = F, row.names = F)


g1 <- ggplot(en.df) + 
  geom_line(aes(sampledate, energy, col = id))+
  geom_point(aes(sampledate, energy, col = id))+
  facet_wrap(~ id, ncol =1) +
  theme_minimal()

g2 <- ggplot(en.df) + 
  geom_line(aes(sampledate, n2, col = id))+
  geom_point(aes(sampledate, n2, col = id))+
  facet_wrap(~ id, ncol =1) +
  theme_minimal()

g1 | g2 + plot_layout(guides = 'collect')


# daphnia
daphnia.df <- read_csv('../Data/max_daphnia_biomass.csv') %>%
  rename(id = lakeid, daphnia = doy, year = year4) %>%
  mutate(decade = year - year%% 3) %>%
  select(id, daphnia, decade, year)
m.daphnia.df <- reshape2::melt(daphnia.df, id.vars = c('id','decade', 'year'))

# ice
ice.df <- read_csv('../Data/ntl_icedatescombo.csv') %>%
  filter(lakeid != 'LR') %>%
  filter(year >= 1979) %>%
  rename(id = lakeid, year = year, iceon = firsticeYDAY, iceoff = lasticeYDAY) %>%
  mutate(decade = year - year %%3) %>%
  select(id, iceon, iceoff, decade, year)
m.ice.df <- reshape2::melt(ice.df, id.vars = c('id','decade', 'year'))

# light
secchi.df <- read_csv('../Data/Secchi_data') %>%
  rename(id = lakeid, year = year4, clearwater = daynum) %>%
  mutate(decade = year - year %%3) %>%
  select(id, clearwater, decade, year)
m.light.df <- reshape2::melt(secchi.df, id.vars = c('id','decade', 'year'))

# chla
chla.df <- read_csv('../Data/chla_epi_max.csv') %>%
  rename(id = lakeid, year = year4, chla = daynum) %>%
  mutate(decade = year - year %%3) %>%
  select(id, chla, decade, year)
m.chla.df <- reshape2::melt(chla.df, id.vars = c('id','decade', 'year'))

# doc
doc.df <- read_csv('../Data/doc.csv') %>%
  filter(!is.na(lakeid)) %>%
  rename(id = lakeid, year = year4, doc = daynum) %>%
  mutate(decade = year - year %%3) %>%
  select(id, doc, decade, year)
m.doc.df <- reshape2::melt(doc.df, id.vars = c('id','decade', 'year'))

c.strat.df = strat.df[c('straton','stratoff','energy','stability', 'anoxia','id', 'year')]
c.strat.df$decade = strat.df$year - strat.df$year%% 3
m.strat.df <- reshape2::melt(c.strat.df, id.vars = c('id','decade', 'year'))

df = rbind(m.strat.df, m.daphnia.df, m.light.df, m.ice.df, m.chla.df, m.doc.df)


df$id <- factor(df$id, levels= (c("AL","BM","CB", "CR","SP", "TB", "TR","FI","ME","MO", "WI")))
df$variable <- factor(df$variable, levels= rev(c("iceoff", "straton", "clearwater", "daphnia", "chla", "doc", "anoxia","stability", "energy","stratoff", "iceon")))

write.csv(df, file ='../Data/phenology_data.csv', quote = F, row.names = F)


