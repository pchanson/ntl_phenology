#devtools::install_github('hdugan/NTLlakeloads')
library(NTLlakeloads)
library("tidyr")
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(data.table)

nuts<-read_csv('C:/Program Files/Git/tmp/ntl_phenology/Data/ntl1_v9_1.csv')
nuts$sampledate<-mdy(nuts$sampledate)

thermo<-read_csv('C:/Program Files/Git/tmp/ntl_phenology/processed/physics/thermocline.csv')
names<-c("sampledate","thermdepth_m","lakeid")
setnames(thermo,names)

merged_data<-merge(nuts,thermo,by=c("lakeid","sampledate")) 

epi<-merged_data[merged_data$depth<1,]

require(plyr)

epi_mean<- epi %>% 
  group_by(lakeid,year4) %>%
  dplyr::summarize_if(is.numeric,mean,na.rm=T)


ggplot(data=epi,aes(x=daynum, y=totpf,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

ggplot(data=epi,aes(x=daynum, y=drp_sloh,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

ggplot(data=epi,aes(x=daynum, y=nh4,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

ggplot(data=epi,aes(x=daynum, y=nh4_sloh,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")
