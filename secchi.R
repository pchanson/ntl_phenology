devtools::install_github('hdugan/NTLlakeloads')
library(NTLlakeloads)
library("tidyr")
library(dplyr)
library(ggplot2)
library(lubridate)

##Secchi

data<-loadLTERsecchi()
ice<-read.csv('C:/Program Files/Git/tmp/ntl_phenology/Data/ntl_icedatescombo.csv')
ice$year4<-year(ice$lastice)

data_combo<-merge(data,ice,by=c("year4","lakeid"))

data_combo$iceoff_doy<-yday(data_combo$lastice)
data_sub<-data_combo[(data_combo$daynum>data_combo$iceoff_doy),]


ggplot(data=data_sub,aes(x=daynum, y=secnview,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

data_sub<-data_sub[(data_sub$daynum<250),]

data_sub$secnview<-as.numeric(as.character(data_sub$secnview))

require(plyr)

max_sec<- data_sub %>% 
  group_by(lakeid,year4) %>%
  slice(which.max(secnview))

ggplot(data=data_sub,aes(x=daynum, y=secnview,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

CWP<-max_sec[(max_sec$lakeid == "AL"|max_sec$lakeid =="BM"|
                 max_sec$lakeid =="ME"|max_sec$lakeid =="MO"),]

ggplot(data=CWP,aes(x=year4,y=daynum))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

##Extinction
library(tidyverse)
library(data.table)
phys<-read.csv('C:/Program Files/Git/tmp/ntl_phenology/Data/ntl29_v10_0.csv')

surf<-phys[(phys$depth<2),]
d1<-surf[(phys$depth==0),]
d1_sub<-d1[, c('lakeid', 'year4', "daynum",'sampledate','depth','light')]

names_0<-c('lakeid', 'year4','daynum', 'sampledate','depth_0','light_0')
setnames(d1_sub, names_0)

d2<-surf[(phys$depth==1),]
d2_sub<-d2[, c('lakeid', 'year4', "daynum",'sampledate','depth','light')]

names_1<-c('lakeid', 'year4','daynum', 'sampledate','depth_1','light_1')
setnames(d2_sub, names_1)

merge_data<-merge(d1_sub, d2_sub, by=c('lakeid', 'year4', 'sampledate','daynum'))
no.na<-na.omit(merge_data)
no.na$ke<-c((log(no.na$light_0)-log(no.na$light_1)))

no.neg<-no.na[no.na$ke>0,]

min_ke<- no.neg %>% 
  group_by(lakeid,year4) %>%
  slice(which.min(ke))

##Time series of ke

ggplot(data=no.neg,aes(x=daynum, y=ke,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

##min ke by year

ggplot(data=min_ke,aes(x=year4,y=daynum))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

#min ke and max secchi over years

ggplot()+
  geom_point(data=min_ke,aes(x=year4,y=daynum),col="black")+
  geom_line(data=min_ke,aes(x=year4,y=daynum),col="black")+
  geom_point(data=max_sec,aes(x=year4,y=daynum),col="red")+
  geom_line(data=max_sec,aes(x=year4,y=daynum),col="red")+
  facet_wrap(~lakeid,scales="free")


######################

lt_ex<-read.csv('C:/Program Files/Git/tmp/ntl_phenology/Data/ntl259_v6.csv')

min_ke_2<- lt_ex %>% 
  group_by(lakeid,year4) %>%
  slice(which.min(extcoef))

ggplot(data=lt_ex,aes(x=daynum, y=extcoef,col=as.factor(year4)))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")

ggplot(data=min_ke_2,aes(x=year4,y=daynum))+
  geom_point()+
  geom_line()+
  facet_wrap(~lakeid,scales="free")


colors <- c("Max Secchi" = "red", "Min ke" = "black")
ggplot()+
  geom_point(data=min_ke_2,aes(x=year4,y=daynum,col="Min Ke"))+
  geom_line(data=min_ke_2,aes(x=year4,y=daynum,col="Min Ke"))+
  geom_point(data=max_sec,aes(x=year4,y=daynum,col="Max Secchi"))+
  geom_line(data=max_sec,aes(x=year4,y=daynum,col="Max Secchi"))+
  facet_wrap(~lakeid,scales="free")+
  scale_colour_manual(name="Line Color",
                      values=c("Max Secchi"="red", "Min Ke"="black"))

