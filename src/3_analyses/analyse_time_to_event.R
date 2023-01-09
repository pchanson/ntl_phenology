library(tidyverse)
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(patchwork)
# library(condSURV)

dat <- read_csv('Data/analysis_ready/final_combined_dates_filled_v2.csv')

# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html

survivalPlot <- function(dataIn, metric1, metric2) {
  df_all = dataIn %>%
    group_by(lakeid, year) %>%
    summarise(time = daynum_fill[which(metric == metric2)] - daynum_fill[which(metric == metric1)],
              status = 1) %>%
    select(lakeid, time, status)
  
  df_eutro = df_all |> filter(lakeid %in% c('ME', 'MO', 'FI')) # ignore Wingra?
  df_dys = df_all |> filter(lakeid %in% c('CB', 'TB'))
  df_oligo = df_all |> filter(lakeid %in% c('AL', 'BM', 'CR', 'SP', 'TB', 'TR'))
  
  s1 <- survfit(Surv(time, status) ~ 1, data = df_eutro)
  s2 <- survfit(Surv(time, status) ~ 1, data = df_oligo)
  s3 <- survfit(Surv(time, status) ~ 1, data = df_dys)
  
  ylabs = paste0('Overall probability of ',metric2,' after ', metric1)
  
  p1 <- ggsurvfit(s1, col = 'green') +
    labs(
      x = "Days",
      y = ylabs,
      title = 'Eutrophic') + 
    add_confidence_interval()+
    add_risktable()
  p2 <- ggsurvfit(s2, col = 'blue') +
    labs(
      x = "Days",
      y = ylabs,
      title = 'Oligotrophic')+ 
    add_confidence_interval()+
    add_risktable()
  p3 <- ggsurvfit(s3, col = 'brown') +
    labs(
      x = "Days",
      y = ylabs,
      title = 'Dystrophic')+ 
    add_confidence_interval()+
    add_risktable()
  
  return(p1 + p2 + p3)
}

survivalPlot(dat, metric1 = 'iceoff', metric2 = 'anoxia_summer')
survivalPlot(dat, metric1 = 'secchi_openwater_min', metric2 = 'secchi_openwater_max')
survivalPlot(dat, metric1 = 'straton', metric2 = 'energy')



df_eutro$state <- 'eutro'
df_oligo$state <- 'oligo'
df_dys$state <- 'dys'
df_anoxia <- (rbind(df_eutro, df_oligo))
df_anoxia <- rbind(df_anoxia, df_dys)

survdiff(Surv(time, status) ~ state, data = df_anoxia)
