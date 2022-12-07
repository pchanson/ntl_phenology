setwd('Projects/DSI/ntl_phenology/')

library(tidyverse)
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(patchwork)
# library(condSURV)

data <- read_csv('Data/analysis_ready/final_combined_dates_filled_v2.csv')

# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html

df_eutro <- data %>%
  filter(lakeid%in% c('ME', 'MO', 'WI', 'FI')) %>%
  group_by(year) %>%
  summarise(time = daynum_fill[which(metric == 'chlor_spring')] - daynum_fill[which(metric == 'iceoff')],
            status = 1) %>%
  select(time, status)
df_oligo <- data %>%
  filter(lakeid%in% c('AL', 'BM', 'CR', 'SP', 'TB', 'TR')) %>%
  group_by(year) %>%
  summarise(time = daynum_fill[which(metric == 'chlor_spring')] - daynum_fill[which(metric == 'iceoff')],
            status = 1) %>%
  select(time, status)
df_dys <- data %>%
    filter(lakeid%in% c('CB', 'TB')) %>%
             group_by(year) %>%
             summarise(time = daynum_fill[which(metric == 'chlor_spring')] - daynum_fill[which(metric == 'iceoff')],
                       status = 1) %>%
             select(time, status)

s1 <- survfit(Surv(time, status) ~ 1, data = df_eutro)
str(s1)
s2 <- survfit(Surv(time, status) ~ 1, data = df_oligo)
s3 <- survfit(Surv(time, status) ~ 1, data = df_dys)

p1 <- ggsurvfit(s1, col = 'green') +
  labs(
    x = "Days",
    y = "Overall probability of spring bloom after ice",
    title = 'Eutrophic'
  ) 
p2 <- ggsurvfit(s2, col = 'blue') +
  labs(
    x = "Days",
    y = "Overall probability of spring bloom after ice",
    title = 'Oligotrophic'
  )
p3 <- ggsurvfit(s3, col = 'brown') +
  labs(
    x = "Days",
    y = "Overall probability of spring bloom after ice",
    title = 'Dystrophic'
  )
p1 + p2 + p3






df_eutro <- data %>%
  filter(lakeid%in% c('ME', 'MO', 'WI', 'FI')) %>%
  group_by(year) %>%
  summarise(time = daynum_fill[which(metric == 'anoxia_summer')] - daynum_fill[which(metric == 'chlor_spring')],
            status = 1) %>%
  select(time, status)
df_oligo <- data %>%
  filter(lakeid%in% c('AL', 'BM', 'CR', 'SP', 'TB', 'TR')) %>%
  group_by(year) %>%
  summarise(time = daynum_fill[which(metric == 'anoxia_summer')] - daynum_fill[which(metric == 'chlor_spring')],
            status = 1) %>%
  select(time, status)
df_dys <- data %>%
  filter(lakeid%in% c('CB', 'TB')) %>%
  group_by(year) %>%
  summarise(time = daynum_fill[which(metric == 'anoxia_summer')] - daynum_fill[which(metric == 'chlor_spring')],
            status = 1) %>%
  select(time, status)

s1 <- survfit(Surv(time, status) ~ 1, data = df_eutro)
str(s1)
s2 <- survfit(Surv(time, status) ~ 1, data = df_oligo)
s3 <- survfit(Surv(time, status) ~ 1, data = df_dys)

p1 <- ggsurvfit(s1, col = 'green') +
  labs(
    x = "Days",
    y = "Overall probability of anoxia after spring bloom",
    title = 'Eutrophic'
  ) 
p2 <- ggsurvfit(s2, col = 'blue') +
  labs(
    x = "Days",
    y = "Overall probability of anoxia after spring bloom",
    title = 'Oligotrophic'
  )
p3 <- ggsurvfit(s3, col = 'brown') +
  labs(
    x = "Days",
    y = "Overall probability of anoxia after spring bloom",
    title = 'Dystrophic'
  )
p1 + p2 + p3
