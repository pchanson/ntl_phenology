# packages
# install.packages('tidyr')
# install.packages('tidyverse')
# install.packages('rLakeAnalyzer')

library(tidyverse)
library(rLakeAnalyzer)
library(lubridate)
library(zoo) # for na.approx
library(pracma) #for function trapz

# remove everything from workspace
rm(list = ls())

# Load NTL LTER long term physical lake data
# Package ID: knb-lter-ntl.29.29 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
inUrl3 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/29/03e232a1b362900e0f059859abe8eb97"
infile3 <- tempfile()
download.file(inUrl3, infile3, method = "auto")

dt1 <- read_csv(infile3) 
# Only use common depths
usedepths = dt1 %>% group_by(depth) %>% tally() %>% filter(n >= 500) %>% pull(depth)
dt1 = dt1 %>% filter(depth %in% usedepths) %>% 
  filter(year4 >= 1982)

get_dens <- function(temp, salt){
  dens = 999.842594 + (6.793952 * 10^-2 * temp) - (9.095290 * 10^-3 * temp^2) +
    (1.001685 * 10^-4 * temp^3) - (1.120083 * 10^-6 * temp^4) + (6.536336 * 10^-9 * temp^5) +
    (8.24493 * 10^-1 -4.0899 * 10^-3 * temp+ 7.6438 * 10^-5 * temp^2 - 8.2467 * 10^-7 * temp^3 + 
       5.3875 * 10^-9* temp^4) * salt+
    (-5.72466 *  10^-3 + 1.0227 * 10^-4 * temp -1.6546 * 10^-6 * temp^2) * salt^(3/2) +
    (4.8314*  10^-4 ) * salt
  return(dens)
}

# Load bathymetry
bath <- read_csv('Data/derived/NTLhypsometry.csv') %>% 
  bind_rows(data.frame('lakeid' = rep('FI',2), 'Depth_m' = c(0,18.9),
                       'Depth_ft' = c(0,0),'area' = c(874000, 0)))

data.temp <- dt1 %>%
  group_by(lakeid, sampledate) %>%
  filter(is.na(flagwtemp), !is.na(wtemp)) %>%
  filter(sum(!is.na(wtemp))>1) %>%
  fill(wtemp, .direction = 'up') %>%
  fill(wtemp, .direction = 'down') %>%
  mutate(iwtemp = na.approx(wtemp)) %>%
  mutate(wdens = get_dens(iwtemp, 0)) %>%
  select(lakeid, year = year4, sampledate, depth, iwtemp, wtemp, wdens)

data.o2 <- dt1 %>%
  group_by(lakeid, sampledate) %>%
  filter(is.na(flagwtemp), !is.na(o2)) %>%
  filter(sum(!is.na(o2))>1) %>%
  fill(o2, .direction = 'up') %>%
  fill(o2, .direction = 'down') %>%
  mutate(io2 = na.approx(o2)) %>%
  select(lakeid, year = year4, sampledate, depth, io2, o2)

data.temp %>% 
  mutate(doy = yday(sampledate)) %>% 
  filter(lakeid == "AL" & year %in% 2007:2017) %>% 
  ggplot(aes(x=doy, y=iwtemp, col=as.factor(depth))) + 
  geom_point() +
  geom_line() + 
  theme_bw() + 
  facet_wrap(~year) +
  theme(legend.position = c(0.9, 0.15)) +
  labs(color="Depth (m)")

data.o2 %>% 
  mutate(doy = yday(sampledate)) %>% 
  filter(lakeid == "WI" & year %in% 2017:2017) %>% 
  ggplot(aes(x=doy, y=io2, col=as.factor(depth))) + 
  geom_point() +
  geom_line() + 
  theme_bw() + 
  facet_wrap(~year) +
  theme(legend.position = c(0.9, 0.15)) +
  labs(color="Depth (m)")
