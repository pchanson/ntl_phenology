# Package ID: knb-lter-ntl.29.8 Cataloging System:https://pasta.lternet.edu.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
# Data set creator:    - Center for Limnology 
# Data set creator:    - NTL LTER 
# Metadata Provider:    - North Temperate Lakes LTER 
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:    - NTL LTER Information Manager University of Wisconsin  - infomgr@lter.limnology.wisc.edu
# Contact:    - NTL LTER Lead PI Center for Limnology  - leadpi@lter.limnology.wisc.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-ntl.29.8
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/8/1932bb71889c8e25cb216c8dc0db33d5" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "wtemp",     
                 "o2",     
                 "o2sat",     
                 "deck",     
                 "light",     
                 "frlight",     
                 "flagdepth",     
                 "flagwtemp",     
                 "flago2",     
                 "flago2sat",     
                 "flagdeck",     
                 "flaglight",     
                 "flagfrlight"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$o2)=="factor") dt1$o2 <-as.numeric(levels(dt1$o2))[as.integer(dt1$o2) ]               
if (class(dt1$o2)=="character") dt1$o2 <-as.numeric(dt1$o2)
if (class(dt1$o2sat)=="factor") dt1$o2sat <-as.numeric(levels(dt1$o2sat))[as.integer(dt1$o2sat) ]               
if (class(dt1$o2sat)=="character") dt1$o2sat <-as.numeric(dt1$o2sat)
if (class(dt1$deck)=="factor") dt1$deck <-as.numeric(levels(dt1$deck))[as.integer(dt1$deck) ]               
if (class(dt1$deck)=="character") dt1$deck <-as.numeric(dt1$deck)
if (class(dt1$light)=="factor") dt1$light <-as.numeric(levels(dt1$light))[as.integer(dt1$light) ]               
if (class(dt1$light)=="character") dt1$light <-as.numeric(dt1$light)
if (class(dt1$frlight)!="factor") dt1$frlight<- as.factor(dt1$frlight)
if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
if (class(dt1$flagwtemp)!="factor") dt1$flagwtemp<- as.factor(dt1$flagwtemp)
if (class(dt1$flago2)!="factor") dt1$flago2<- as.factor(dt1$flago2)
if (class(dt1$flago2sat)!="factor") dt1$flago2sat<- as.factor(dt1$flago2sat)
if (class(dt1$flagdeck)!="factor") dt1$flagdeck<- as.factor(dt1$flagdeck)
if (class(dt1$flaglight)!="factor") dt1$flaglight<- as.factor(dt1$flaglight)
if (class(dt1$flagfrlight)!="factor") dt1$flagfrlight<- as.factor(dt1$flagfrlight)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year4)
summary(daynum)
summary(sampledate)
summary(depth)
summary(rep)
summary(sta)
summary(event)
summary(wtemp)
summary(o2)
summary(o2sat)
summary(deck)
summary(light)
summary(frlight)
summary(flagdepth)
summary(flagwtemp)
summary(flago2)
summary(flago2sat)
summary(flagdeck)
summary(flaglight)
summary(flagfrlight) 
# Get more details on character variables

summary(as.factor(dt1$lakeid)) 
summary(as.factor(dt1$rep)) 
summary(as.factor(dt1$sta)) 
summary(as.factor(dt1$event)) 
summary(as.factor(dt1$frlight)) 
summary(as.factor(dt1$flagdepth)) 
summary(as.factor(dt1$flagwtemp)) 
summary(as.factor(dt1$flago2)) 
summary(as.factor(dt1$flago2sat)) 
summary(as.factor(dt1$flagdeck)) 
summary(as.factor(dt1$flaglight)) 
summary(as.factor(dt1$flagfrlight))
detach(dt1)               

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

bath <- read.csv('Projects/DSI/ntl_phenology/Data/NTLhypsometry.csv')
bath <- rbind(bath, data.frame('lakeid' = rep('FI',2), 'Depth_m' = c(0,18.9),
                               'Depth_ft' = c(0,0),'area' = c(874000, 0)))

ntl.id <- unique(dt1$lakeid)
strat.df <- data.frame('year' = NULL, 'straton' = NULL, 'stratoff' = NULL, 'duration' = NULL,
                       'energy' = NULL, 'stability' = NULL, 'anoxia' = NULL, 'id' = NULL)
en.df <- data.frame('sampledate' = NULL, 'energy' = NULL, 'n2' = NULL, 'id' = NULL)

therm.df <- data.frame('sampledate' = NULL, 'thermdepth_m' = NULL, 'id' = NULL)

for (name in ntl.id){
  
  data <- dt1 %>%
    filter(lakeid == name) %>%
    group_by(sampledate) %>%
    filter((flagwtemp) == "") %>%
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
    filter((flagwtemp) == "") %>%
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
    
    an <- data.o2 %>%
      filter(year4 == a) %>%
      group_by(sampledate) %>%
      arrange(depth) %>%
      summarise(z = seq(min(depth),max(depth),dz),
                area = approx(hyp$Depth_m, hyp$area, seq(min(depth), max(depth),dz))$y,
                do = approx(depth, o2, seq(min(depth), max(depth), dz))$y) %>%
      filter(z >= border & !is.na(do)) %>%
      summarise('do' = abs(trapz(z * area, do)))
    
    df = df %>% mutate(densdiff = ifelse(densdiff > 0.1 & surfwtemp >= 4, densdiff, NA))
    
    df <- df[complete.cases(df),]
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
write.csv(therm.df, file ='Projects/DSI/ntl_phenology/Data/thermocline.csv', quote = F, row.names = F)


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
daphnia.df <- read_csv('Projects/DSI/ntl_phenology/Data/max_daphnia_biomass.csv') %>%
  rename(id = lakeid, daphnia = doy, year = year4) %>%
  mutate(decade = year - year%% 3) %>%
  select(id, daphnia, decade, year)
m.daphnia.df <- reshape2::melt(daphnia.df, id.vars = c('id','decade', 'year'))

# ice
ice.df <- read_csv('Projects/DSI/ntl_phenology/Data/ntl_icedatescombo.csv') %>%
  filter(lakeid != 'LR') %>%
  filter(year >= 1979) %>%
  rename(id = lakeid, year = year, iceon = firsticeYDAY, iceoff = lasticeYDAY) %>%
  mutate(decade = year - year %%3) %>%
  select(id, iceon, iceoff, decade, year)
m.ice.df <- reshape2::melt(ice.df, id.vars = c('id','decade', 'year'))

# light
secchi.df <- read_csv('Projects/DSI/ntl_phenology/Data/Secchi_data') %>%
  rename(id = lakeid, year = year4, clearwater = daynum) %>%
  mutate(decade = year - year %%3) %>%
  select(id, clearwater, decade, year)
m.light.df <- reshape2::melt(secchi.df, id.vars = c('id','decade', 'year'))

# chla
chla.df <- read_csv('Projects/DSI/ntl_phenology/chla_epi_max.csv') %>%
  rename(id = lakeid, year = year4, clearwater = daynum) %>%
  mutate(decade = year - year %%3) %>%
  select(id, clearwater, decade, year)
m.light.df <- reshape2::melt(secchi.df, id.vars = c('id','decade', 'year'))

c.strat.df = strat.df[c('straton','stratoff','energy','stability', 'anoxia','id', 'year')]
c.strat.df$decade = strat.df$year - strat.df$year%% 3
m.strat.df <- reshape2::melt(c.strat.df, id.vars = c('id','decade', 'year'))

df = rbind(m.strat.df, m.daphnia.df, m.light.df, m.ice.df)

ggplot(df) + 
  geom_density(aes(x = value, col = variable, fill = variable), alpha = 0.5) +
  facet_wrap(~ factor(id)) +
  xlab('DOY') + ylab('Density')+
  theme_minimal() 

df$id <- factor(df$id, levels= (c("AL","BM","CB", "CR","SP", "TB", "TR","FI","ME","MO", "WI")))
df$variable <- factor(df$variable, levels= rev(c("iceoff", "straton", "clearwater", "daphnia", "stability", "anoxia", "energy","stratoff", "iceon")))

g <- ggplot(df) + 
  stat_density_ridges(aes(x = as.Date(value, origin = as.Date('2019-01-01')), 
                          y= variable, col = variable, fill = variable), 
                      alpha = 0.5, quantile_lines = T, quantiles = 2) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~ (id)) +
  xlab('DOY') + ylab('Density')+
  theme_minimal() ; g
ggsave(file = 'Projects/DSI/ntl_phenology/Figures/phenology.png', g, dpi = 500, width =9, height = 8)

ggplot(subset(df, id == 'ME' & year >=1995)) + 
    stat_density_ridges(aes(x = value, y= variable, col = variable, fill = variable), 
                        alpha = 0.5, quantile_lines = T, quantiles = 2) +
    facet_wrap(~ factor(decade)) +
    xlab('DOY') + ylab('Density')+
    theme_minimal() 

nx = 3
for (i in unique(df$year)[1:(length(unique(df$year))-nx)]){
  g <- ggplot(subset(df, year %in% c(i:(i+nx)))) + 
    geom_density(aes(x = value, col = variable, fill = variable), alpha = 0.5) +
    facet_wrap(~ factor(id)) +
    xlab('DOY') + ylab('Density')+
    theme_minimal() + 
    ggtitle(paste0(i)); g
  
  ggsave(file = paste0('Projects/DSI/ntl_phenology/processed/physics/pheno_',i,'.png'), g, dpi = 500, width =9, height = 8)
}