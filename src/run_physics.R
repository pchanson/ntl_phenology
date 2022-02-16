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
   

# high-frequency data
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/29/63d0587cf326e83f57b054bf2ad0f7fe" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "sampledate",     
                 "year4",     
                 "month",     
                 "daynum",     
                 "hour",     
                 "depth",     
                 "wtemp",     
                 "flag_wtemp"    ), check.names=TRUE)

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

strat.df_hf <- data.frame('year' = NULL, 'straton' = NULL, 'stratoff' = NULL, 'duration' = NULL,
                       'energy' = NULL, 'stability' = NULL, 'id' = NULL)
en.df_hf <- data.frame('sampledate' = NULL, 'energy' = NULL, 'n2' = NULL, 'id' = NULL)

therm.df_hf <- data.frame('sampledate' = NULL, 'thermdepth_m' = NULL, 'id' = NULL)

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
  
  if (name == 'ME'){
    data_hf <- dt2 %>%
      mutate(sampledate = as.POSIXct(sampledate)) %>%
      group_by(sampledate) %>%
      mutate(flagwtemp = ifelse(length(dt2$flag_wtemp[1]) == 1, NA, 999)) %>%
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
  }

  
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
    
    if (name == 'ME' && (a %in% data_hf$year4)){
      df_hf <- data_hf %>%
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
      
      therm.df_hf <- rbind(therm.df_hf, data.frame('sampledate' = df_hf$sampledate, 'thermdepth_m' = df_hf$thermdep, 'id' = name))
      
      border <- floor(mean(df_hf$thermdep, na.rm = T))
      
      dz = 0.1
      en_hf <- data_hf %>%
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
      
      
      df_hf = df_hf %>% mutate(densdiff = ifelse(densdiff > 0.1 & surfwtemp >= 4, densdiff, NA))
      
      df_hf <- df_hf[complete.cases(df_hf),]
      
      
      strat.df_hf <- rbind(strat.df_hf, data.frame('year' = a,
                                             'straton' = yday(df_hf$sampledate[which.min(df_hf$sampledate)]),
                                             'stratoff' = yday(df_hf$sampledate[which.max(df_hf$sampledate)]),
                                             'duration' = yday(df_hf$sampledate[which.max(df_hf$sampledate)]) - yday(df_hf$sampledate[which.min(df_hf$sampledate)]),
                                             'energy' = yday(en_hf$sampledate[which.max(en_hf$energy)]),
                                             'stability' = yday(en_hf$sampledate[which.max(en_hf$n2max)]),
                                             'id' = name))
      en.df_hf <- rbind(en.df_hf, data.frame('sampledate' = en_hf$sampledate, 'energy' = en_hf$energy, 'n2' = en_hf$n2max,
                                       id = rep(name, nrow(en_hf))))
    }
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

# physics
c.strat.df = strat.df[c('straton','stratoff','energy','stability', 'anoxia','id', 'year')]
c.strat.df$decade = strat.df$year - strat.df$year%% 3
m.strat.df <- reshape2::melt(c.strat.df, id.vars = c('id','decade', 'year'))

# hf physics
c.strat.df_hf = strat.df_hf[c('straton','stratoff','energy','stability', 'id', 'year')]
c.strat.df_hf$decade = strat.df_hf$year - strat.df_hf$year%% 3
m.strat.df_hf <- reshape2::melt(c.strat.df_hf, id.vars = c('id','decade', 'year'))

# combine everything
df = rbind(m.strat.df, m.daphnia.df, m.light.df, m.ice.df, m.chla.df, m.doc.df)


df$id <- factor(df$id, levels= (c("AL","BM","CB", "CR","SP", "TB", "TR","FI","ME","MO", "WI")))
df$variable <- factor(df$variable, levels= rev(c("iceoff", "straton", "clearwater", "daphnia", "chla", "doc", "anoxia","stability", "energy","stratoff", "iceon")))

df_hf = rbind(m.strat.df_hf)

df_hf$id <- factor(df_hf$id, levels= (c("ME")))
df_hf$variable <- factor(df_hf$variable, levels= rev(c("straton", "stability", "energy","stratoff")))

# compare biweekly physics with hf physics
df_comp1 <- df %>% 
  filter(id == 'ME') %>%
  mutate(id = 'ME_biweekly')
df_comp2 <- df_hf %>%
  mutate(id = 'ME_hf')
df_comp <- rbind(df_comp1, df_comp2)

ggplot(df_comp) + 
  stat_density_ridges(aes(x = as.Date(value, origin = as.Date('2019-01-01')), 
                          y= variable, col = variable, fill = variable), 
                      alpha = 0.5, quantile_lines = T, quantiles = 2) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~ (id)) +
  xlab('') + ylab('Density')+
  theme_minimal() 


write.csv(df, file ='../Data/phenology_data.csv', quote = F, row.names = F)


