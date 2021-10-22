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
library(RColorBrewer)
library(corrplot)

all.vars = c('straton', 'stratoff', 'energy', 'stability', 'anoxia',
             'iceon', 'iceoff', 'daphnia', 'clearwater', 'chla', 'doc')

df <- read_csv('../Data/phenology_data.csv') %>%
  filter(variable != 'clearwater')

df <- read_csv('../Data/phenology_data.csv') %>%
  filter(variable %in% c('straton', 'stratoff', 'energy', 'stability', 'anoxia',
                         'iceon', 'iceoff'))

df <- read_csv('../Data/phenology_data.csv') %>%
  filter(variable %in% c('straton', 'stratoff', 'energy', 'stability', 'anoxia',
                         'iceon', 'iceoff', 'daphnia', 'chla', 'doc'))

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
    x <- abs(x)
    error <- try(eigen(x), silent = T)
    if (class(error) == "try-error"){
      sim.df <- rbind(sim.df, data.frame('year' = i, 
                                         'id' = d,
                                         'eigenvalue' = NA,
                                         'eigenvector' = NA))
    } else {
      
      sim.df <- rbind(sim.df, data.frame('year' = i, 
                                         'id' = d,
                                         'eigenvalue' = max(eigen(x)$values),
                                         'eigenvector' = max(eigen(x)$vector)))
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
  geom_point(aes(year, scale(as.numeric(eigenvalue)), col =id, group = id)) +
  geom_line(aes(year, scale(as.numeric(eigenvalue)), col = id, group = id)) +
  # geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)+
  # facet_wrap(~ id, ncol = 1)+
  scale_color_brewer(palette = "Paired") +
  theme_bw()

ggplot(sim.df %>% arrange(year), aes(year, (as.numeric(eigenvector)))) +
  geom_point(aes(year, scale(as.numeric(eigenvector)), col =id, group = id)) +
  geom_line(aes(year, scale(as.numeric(eigenvector)), col = id, group = id)) +
  # geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)+
  # facet_wrap(~ id, ncol = 1)+
  scale_color_brewer(palette = "Paired") +
  theme_bw()

d.sim.df = sim.df %>% 
  group_by(id) %>%
  mutate(diff = c(0,diff((eigenvalue))))

ggplot(d.sim.df %>% arrange(year)) +
  geom_point(aes(year, as.numeric((diff)), group = id, col =id)) +
  geom_line(aes(year, as.numeric((diff)), group = id, col =id)) +
  scale_color_brewer(palette = "Paired") +
  # facet_wrap(~ id, ncol = 1)+
  theme_bw()

ggplot(d.sim.df %>% arrange(year)) +
  geom_point(aes(year, (as.numeric(diff)), group = id, col =id)) +
  geom_line(aes(year, (as.numeric(diff)), group = id, col =id)) +
  facet_wrap(~ id, ncol = 1)+
  theme_bw()


lake.id = c('AL', 'BM','CR', 'SP','TR','CB', 'TB', 'FI', 'ME', 'MO', 'WI')
corr.m <- matrix(NA, nrow = length(lake.id), ncol = length(lake.id))
for (m in lake.id){
  for (n in lake.id){
    dit = sim.df %>% filter(id == m) %>% arrange(year)
    dat =  sim.df %>% filter(id == n) %>% arrange(year)
    error <- try(cor(dit$eigenvalue, dat$eigenvalue,  method = "pearson", use = "complete.obs"), silent = T)
    if (class(error) == "try-error"){
      pear = NaN
    }else{
      pear = cor(dit$eigenvalue, dat$eigenvalue,  method = "pearson", use = "complete.obs")
    }
    g <- ggplot() +
      geom_point(data = sim.df %>% filter(id == m) %>% arrange(year),aes(year, (as.numeric(eigenvalue)), col=m, group = id)) +
      geom_line(data = sim.df %>% filter(id == m) %>% arrange(year), aes(year, (as.numeric(eigenvalue)), col = m, group = id)) +
      # geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)+
      # facet_wrap(~ id, ncol = 1)+
      geom_point(data = sim.df %>% filter(id == n) %>% arrange(year),aes(year, (as.numeric(eigenvalue)), col =n, group = id)) +
      geom_line(data = sim.df %>% filter(id == n) %>% arrange(year), aes(year, (as.numeric(eigenvalue)), col = n, group = id)) +
      ylab('eigenvalue') + xlab('')+
      scale_color_brewer(palette = "Set2") +
      theme_bw() +
      ggtitle(paste0('Pearson ', pear))
    corr.m[match(m,lake.id),match(n,lake.id)] = pear
    ggsave(file = paste0('../Figures/eigenvalues/',m,'_',n,'.png'), g, dpi = 500, width =9, height = 3)
  }
}

str(corr.m)

colnames(corr.m) <- lake.id
rownames(corr.m) <- lake.id
corrplot(corr.m, method="circle")
