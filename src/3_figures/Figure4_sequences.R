# setwd('Projects/DSI/ntl_phenology/')

library(tidyverse)
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(patchwork)
library(ggpubr)
library(ggpval)
library(pacman)
library(TraMineR)
library(MetBrewer)
# library(condSURV)

data <-  read_csv('Data/analysis_ready/final_combined_dates_filled_v2.csv')

head(data)

df <- data %>%
  mutate(trophic = ifelse(lakeid %in% c('ME', 'MO'), 'eutrophic',
                          ifelse(lakeid %in% c('CB', 'TB'), 'dystrophic', 
                                 ifelse(lakeid %in% c('CR', 'SP'), 'oligotrophic',
                                        'rest')))) %>%
  group_by(trophic, metric) %>%
  mutate(daynum_centered = daynum_fill - mean(daynum_fill, na.rm = T),
         mean_trophic = mean(daynum_fill, na.rm = T))



# Load the depmixS4 package
library(depmixS4)
# library(MmgraphR)


# install.packages('seqHMM')
#https://cran.r-project.org/web/packages/seqHMM/vignettes/seqHMM.pdf
# library(devtools)
# install_github("helske/seqHMM")
library(seqHMM)


###############################
diss <- c()
sequencePlots = list()
costs <- list()

# df_red=df
df_red = df %>%
  filter(metric %in% c('iceoff', 'iceon', 'straton', 'stratoff', 'stability', 'anoxia_summer', 'secchi_max')) |> 
  mutate(metric = factor(metric, levels = c( 'iceoff', 'straton','secchi_max','stability', 'anoxia_summer', 'stratoff','iceon')))

#'secchi_min', 
#'zoopDensity'))

metric_n <- length(unique(df_red$metric))

data = c()
data_plot = c()

for (lakenames in unique(df_red$lakeid)){
    data_HMM = df_red %>%
      filter(lakeid == lakenames) %>%
      mutate(year_norm = year - min(year),
             time_norm = daynum_fill + year_norm * 365) %>%
      rename(event = metric,
             time = time_norm,
             hidden_state = year_norm) %>%
      mutate(event = as.factor(event),
             hidden_state = as.factor(year)) %>%
      arrange(hidden_state, time) %>%
      # group_by(lakeid) %>%
      mutate(event = as.numeric(event)) %>%
      dplyr::select(event, hidden_state)
    data_HMM <- data_HMM[,-1]
    
    data_HMM$event_id =  rep(paste0('event_',seq(1,metric_n)), nrow(data_HMM)/metric_n)
    
    data_HMM_plot = df_red %>%
      filter(lakeid == lakenames) %>%
      mutate(year_norm = year - min(year),
             time_norm = daynum_fill + year_norm * 365) %>%
      rename(event = metric,
             time = time_norm,
             hidden_state = year_norm) %>%
      mutate(event = as.factor(event),
             hidden_state = as.factor(year)) %>%
      arrange(hidden_state, time) %>%
      # group_by(lakeid) %>%
      # mutate(event = as.numeric(event)) %>%
      dplyr::select(event, hidden_state)
    data_HMM_plot <- data_HMM_plot[,-1]
    
    data_HMM_plot$event_id =  rep(paste0('event_',seq(1,metric_n)), nrow(data_HMM)/metric_n)
    
    data_plot = rbind(data_plot, data_HMM_plot)
    
    data_HMM = data_HMM %>%
      group_by(hidden_state) %>%
      pivot_wider(names_from = event_id, values_from = event)
    
    data =as.data.frame(data_HMM[, -1])
  
  pheno_seq <- seqdef(data, start = 1, labels = levels(df_red$metric))
  

  
  
  seq <- seqdef(data)
  seqdplot(seq)
  
  trate = seqtrate(seq)
  heatTrate = reshape2::melt(trate)
  # ggplot(heatTrate, aes(Var2, Var1)) +
  #   geom_tile(aes(fill = value))
  # 
  # seqHtplot(seq, with.legend = "right", legend.prop=0.4)
  # scost <- seqsubm(seq, method = "TRATE")
  scost <- seqsubm(seq, method="CONSTANT", cval=2)
  mean_data = data_plot %>% group_by(event_id) %>% count(event) %>% filter(n==max(n))
  mean_data_seq = as.data.frame(t(as.matrix(match(mean_data$event,  levels(df_red$metric)))))
  colnames(mean_data_seq) = colnames(data)
  mean_seq = seqdef(mean_data_seq)
  
  # https://medium.com/@surfingthroughlifeasapostdoc/sequence-analysis-time-use-data-atus-in-r-760d80b8d08b
  om_time <- seqdist(seq, method = "OM", indel = 1, sm = scost, full.matrix = FALSE)
  # seqdist(seq, method = 'OM', sm = seqsubm(seq, method="CONSTANT", cval=2))
  om_time <- om_time#/max(om_time)
  round(om_time, 1)
  
  medoid <- seqrep(seq, diss = om_time, criterion = 'dist', nrep = 1)
  print(medoid, format = 'SPS')
  # seqrplot(seq, diss = om_time, border = NA)
  
  costs[[match(lakenames, unique(df_red$lakeid))]] = as.vector(om_time)
  
  diss =append(diss, c(round(mean(om_time),2)))
  
  sequencePlots[[lakenames]] <-
    ggplot(data = data_plot %>% group_by(event_id) %>% count(event),
           aes(x = event_id, y = (n*100)/max(n), fill = event)) +
    scale_fill_manual(values = rev(met.brewer("Redon", metric_n)), name = 'metric') + 
    scale_y_continuous(expand = c(0,0)) +
    geom_bar(stat = 'identity') + ylab('Occurrence (%)') +
    xlab('') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 6),
          plot.title = element_text(size = 8)) +
    ggtitle(paste0(lakenames,': ',round(mean(om_time),2)))
  
}



p.out = (sequencePlots[['BM']] +   sequencePlots[['TR']]) /
(sequencePlots[['CR']] +   sequencePlots[['SP']]) /
(sequencePlots[['CB']] +   sequencePlots[['TB']]) /
  (sequencePlots[['ME']] +   sequencePlots[['MO']]) /
(sequencePlots[['AL']] +   sequencePlots[['WI']]) +  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom',
        legend.margin=margin(t = 0, unit='cm'),
        legend.key.size = unit(0.3,'cm')) & 
  guides(fill = guide_legend(nrow = 1))

ggsave(p.out, filename = 'Figures_manuscript/Figure4.png', dpi = 500, width = 6, height = 6)



# ggplot() +
#   geom_density(data = as.data.frame(costs[[2]]), aes(x = costs[[2]])) + 
#   geom_density(data = as.data.frame(costs[[7]]), aes(x = costs[[7]])) + 
#   geom_density(data = as.data.frame(costs[[4]]), aes(x = costs[[4]])) + 
#   geom_density(data = as.data.frame(costs[[5]]), aes(x = costs[[5]]))  
# 
# ggplot() +
#   geom_density(data = as.data.frame(costs[[3]]), aes(x = costs[[3]])) +
#   geom_density(data = as.data.frame(costs[[6]]), aes(x = costs[[6]])) 
# 
# ggplot() +
#   geom_density(data = as.data.frame(costs[[1]]), aes(x = costs[[1]])) +
#   geom_density(data = as.data.frame(costs[[10]]), aes(x = costs[[10]])) 
# ggplot() +
#   geom_density(data = as.data.frame(costs[[8]]), aes(x = costs[[8]])) +
#   geom_density(data = as.data.frame(costs[[9]]), aes(x = costs[[9]])) 
#   
  