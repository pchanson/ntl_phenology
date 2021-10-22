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

n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

df <- read_csv('../Data/phenology_data.csv')

str(df)

library(kohonen)

df.s <- df %>%
  filter(variable%in%c('straton', 'stratoff', 'energy', 'stability','anoxia','daphnia','iceon',
                      'iceoff', 'chla'))

df.wide <- pivot_wider(df.s, names_from = variable,
                       values_from = value)


idx <- df.wide[complete.cases(df.wide),]

df.matrix <- as.matrix(idx[c('straton', 'stratoff', 'energy', 'stability','anoxia','daphnia','iceon',
                                 'iceoff', 'chla')])

names <- paste0(idx$id)#,'_',df.wide$year)
n.colors <- c(rep(col_vector[1],sum(!is.na(match(names, 'AL')))),
              rep(col_vector[2],sum(!is.na(match(names, 'BM')))),
              rep(col_vector[3],sum(!is.na(match(names, 'CB')))),
              rep(col_vector[4],sum(!is.na(match(names, 'CR')))),
              rep(col_vector[5],sum(!is.na(match(names, 'FI')))),
              rep(col_vector[6],sum(!is.na(match(names, 'ME')))),
              rep(col_vector[7],sum(!is.na(match(names, 'MO')))),
              rep(col_vector[8],sum(!is.na(match(names, 'SP')))),
              rep(col_vector[9],sum(!is.na(match(names, 'TB')))),
              rep(col_vector[10],sum(!is.na(match(names, 'TR')))),
              rep(col_vector[11],sum(!is.na(match(names, 'WI')))))

set.seed(222)
g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular" )

map <- som(df.matrix,
           grid = g,
           alpha = c(0.05, 0.01),
           radius = 1)

plot(map, type='codes',palette.name = rainbow, labels = names, )

plot(map, type='mapping',col = n.colors,
     label = names,pchs = names)
