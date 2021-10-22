# remove everything from workspace
rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(rLakeAnalyzer)
library(lubridate)
library(zoo)
library(patchwork)
library(gganimate)
library(ggridges)
library(pracma)
library(scales)
library(Matrix)


df <- read_csv('../Data/phenology_data.csv') %>%
  filter(variable != 'clearwater')

df <- read_csv('../Data/phenology_data.csv') %>%
  filter(variable %in% c('straton', 'stratoff', 'energy', 'stability', 'anoxia',
                         'iceon', 'iceoff'))

str(df)


df.space <- list()
sim.df <- data.frame('year' = NULL, 'id' = NULL, 'eigenvalue' = NULL,
           'eigenvector' = NULL)#, 'det' = NULL, 'rank' = NULL)
           #'
lake.id = unique(df$id)
# across time
for (d in lake.id){
  for (i in unique(df$year)){
    data <- df %>% filter(id == d, year == i)
    x <- matrix(NA, ncol=length( unique(df$variable)), nrow=length( unique(df$variable)))
    for (n in (unique(df$variable))){
      for (m in (unique(df$variable))){
        x[match(n,(unique(df$variable))), match(m,(unique(data$variable)))] = data$value[match(n,unique(data$variable))] - data$value[match(m,unique(data$variable))]
      }
    }
    
    error <- try(eigen(x), silent = T)
    if (class(error) == "try-error"){
      sim.df <- rbind(sim.df, data.frame('year' = i, 
                                         'id' = d,
                                         'eigenvalue' = NA,
                                         'eigenvector' = NA))
    } else {
      
      sim.df <- rbind(sim.df, data.frame('year' = i, 
                                         'id' = d,
                                         'eigenvalue' = sum(eigen(x)$values),
                                         'eigenvector' = sum(eigen(x)$vector)))
    }
    
    df.space[[match(i,unique(df$year))]] = x
    
  }
}


ggplot(sim.df %>% arrange(year)) +
  geom_point(aes(year, (as.numeric(eigenvalue)))) +
  geom_line(aes(year, (as.numeric(eigenvalue)))) +
  facet_wrap(~ id, ncol = 1)+
  theme_bw()

ggplot(sim.df %>% arrange(year)) +
  geom_point(aes(year, (as.numeric(eigenvector)))) +
  geom_line(aes(year, (as.numeric(eigenvector)))) +
  facet_wrap(~ id, ncol = 1)+
  theme_bw()

ggplot(sim.df %>% arrange(year), aes(year, (as.numeric(eigenvalue)))) +
  geom_point(aes(year, (as.numeric(eigenvalue)), col =id)) +
  geom_line(aes(year, (as.numeric(eigenvalue)), col = id)) +
  geom_smooth()+
  # facet_wrap(~ id, ncol = 1)+
  theme_bw()
