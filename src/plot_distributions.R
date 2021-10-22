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

df <- read_csv('../Data/phenology_data.csv')

df$id <- factor(df$id, levels= (c("AL","BM","CB", "CR","SP", "TB", "TR","FI","ME","MO", "WI")))
df$variable <- factor(df$variable, levels= rev(c("iceoff", "straton", "clearwater", "daphnia", "chla", "doc", "anoxia","stability", "energy","stratoff", "iceon")))

g <- ggplot(df) + 
  stat_density_ridges(aes(x = as.Date(value, origin = as.Date('2019-01-01')), 
                          y= variable, col = variable, fill = variable), 
                      alpha = 0.5, quantile_lines = T, quantiles = 2) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~ (id)) +
  xlab('') + ylab('Density')+
  theme_minimal() ; g

ggsave(file = '../Figures/phenology.png', g, dpi = 500, width =9, height = 8)
ggsave(file = '../Figures/phenology.pdf', g, width =9, height = 8)


# ggplot(subset(df, id == 'ME' & year >=1995)) + 
#     stat_density_ridges(aes(x = value, y= variable, col = variable, fill = variable), 
#                         alpha = 0.5, quantile_lines = T, quantiles = 2) +
#     facet_wrap(~ factor(decade)) +
#     xlab('DOY') + ylab('Density')+
#     theme_minimal() 

# nx = 3
# for (i in unique(df$year)[1:(length(unique(df$year))-nx)]){
#   g <- ggplot(subset(df, year %in% c(i:(i+nx)))) + 
#     geom_density(aes(x = value, col = variable, fill = variable), alpha = 0.5) +
#     facet_wrap(~ factor(id)) +
#     xlab('DOY') + ylab('Density')+
#     theme_minimal() + 
#     ggtitle(paste0(i)); g
#   
#   ggsave(file = paste0('Projects/DSI/ntl_phenology/processed/physics/pheno_',i,'.png'), g, dpi = 500, width =9, height = 8)
# }