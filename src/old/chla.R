#devtools::install_github('hdugan/NTLlakeloads')
library(NTLlakeloads)
library("tidyr")
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(data.table)

data1<-read_csv('C:/Program Files/Git/tmp/ntl_phenology/Data/ntl34_v4_1.csv')
data2<-read_csv('C:/Program Files/Git/tmp/ntl_phenology/Data/ntl38_v5.csv')


data1<-data1[,c("lakeid","year4","daynum","sampledate","depth","chlor")]
data1$sampledate<-mdy(data1$sampledate)
data1$daynum<-yday(data1$sampledate)
data2<-data2[,c("lakeid","year4","sampledate","depth_range_m","correct_chl_fluor")]
data2$sampledate<-mdy(data2$sampledate)
data2$daynum<-yday(data2$sampledate)


data1_sub<-data1[data1$depth<2,]
data2_sub<-data2[data2$depth_range_m == "0-2",]

require(plyr)

data1_mean<- data1_sub %>% 
  group_by(lakeid,sampledate) %>%
  dplyr::summarize_if(is.numeric,mean,na.rm=T)

data1_max<- data1_sub %>% 
  group_by(lakeid,year4) %>%
  slice(which.max(chlor))

data2_mean<- data2_sub %>% 
  group_by(lakeid,sampledate) %>%
  dplyr::summarize_if(is.numeric,mean,na.rm=T)

data2_max<- data2_sub %>% 
  group_by(lakeid,year4) %>%
  slice(which.max(correct_chl_fluor))

ggplot(data=data1_max,aes(x=year4,y=daynum))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

ggplot(data=data2_max,aes(x=year4,y=daynum))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")


ggplot(data=data2_max,aes(x=daynum))+
  geom_density()+
  facet_wrap(~lakeid,scales="free")

data_check<-data1_sub[data1_sub$lakeid=="AL",]

ggplot(data=data_check,aes(x=daynum, y=chlor,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(year4~.,scales="free")

data_check<-data2_sub[data2_sub$lakeid=="ME",]

ggplot(data=data_check,aes(x=daynum, y=correct_chl_fluor,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(year4~.,scales="free")

##########################

ggplot(data=data1_mean,aes(x=daynum, y=chlor,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")


ggplot(data=data2_mean,aes(x=daynum, y=correct_chl_fluor,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")


data1_dcl<-data1[data1$depth>5,]

data1_dcl_mean<- data1_dcl %>% 
  group_by(lakeid,sampledate) %>%
  dplyr::summarize_if(is.numeric,mean,na.rm=T)

ggplot(data=data1_dcl_mean,aes(x=daynum, y=chlor,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

names<-c("lakeid","year4","sampledate","depth","chlor","daynum")
setnames(data2_max, names)

data2_max <- data2_max[, c("lakeid","year4","daynum","sampledate","depth","chlor")]

data1_max$depth<-as.character(data1_max$depth)
all_chla_data<-rbind(data1_max,data2_max)

#write.csv(all_chla_data, "chla_epi_max.csv")
