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

source('coolBlueHotRed.R')
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

#https://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/



data_train_matrix <- as.matrix(scale(df.matrix))

som_grid <- somgrid(xdim = 5, ydim=5, topo="hexagonal")

som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

layout(1)
plot(som_model, type="changes")

plot(som_model, type="count")

plot(som_model, type="codes")

layout(matrix(1:9,ncol=3,nrow=3,byrow=T))

for(i in 1:9){
  plot(som_model, type = "property", property=som_model$codes[[1]][,i], main=sprintf(
    c('straton', 'stratoff', 'energy', 'stability','anoxia','daphnia','iceon','iceoff', 'chla')[i]), palette.name=coolBlueHotRed)
}

for(i in 1:9){
  var <- i #define the variable to plot 
  var_unscaled <- aggregate(as.numeric(df.matrix[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
  plot(som_model, type = "property", property=var_unscaled, 
       main=sprintf(
         c('straton', 'stratoff', 'energy', 'stability','anoxia','daphnia','iceon','iceoff', 'chla')[i]), 
       palette.name=coolBlueHotRed)
}

mydata <- som_model$codes 
wss <- (nrow(mydata)-1)*sum(apply(mydata[[1]],2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata[[1]], centers=i)$withinss)
}
plot(wss)

## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])),7)
# plot these results:
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', "#7f7f7f", "#bcbd22", "#17becf","#00FF00", "#9F81F7"
                    ,"#FFFF00", "#F6CECE","#610B21")
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters",
     label = names,pchs = names) 
add.cluster.boundaries(som_model, som_cluster)
