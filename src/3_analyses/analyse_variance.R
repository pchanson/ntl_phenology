setwd('C:/Users/kreinl1/OneDrive/OneDrive - UW-Madison/GitHub/ntl_phenology/Data/analysis_ready')

library(tidyverse)
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(patchwork)
library(ggpubr)
library(ggpval)
# library(condSURV)

data <- read_csv('final_combined_dates_filled_v2.csv')

head(data)

df <- data %>%
  mutate(trophic = ifelse(lakeid %in% c('FI', 'ME', 'MO', 'WI'), 'eutrophic',
                          ifelse(lakeid %in% c('CB', 'TB'), 'dystrophic', 'oligotrophic'))) %>%
  group_by(trophic, metric) %>%
  mutate(daynum_centered = daynum_fill - mean(daynum_fill, na.rm = T),
         mean_trophic = mean(daynum_fill, na.rm = T))

var.test(daynum_centered ~ trophic, df)

straton<-df %>% 
  filter(metric=="straton")

straton_kw<-kruskal.test(daynum_centered ~ trophic, data = straton)
summary(straton_kw)
straton_kw$p.value
pairwise.wilcox.test(straton$daynum_centered, straton$trophic,
                     p.adjust.method = "BH")


analysis_df<-data.frame(c(""))



ggplot(df) +
  geom_density(aes(daynum_fill - mean(daynum_fill, na.rm = T), group = trophic, fill = trophic), alpha = 0.5) +
  facet_wrap(~ metric)

ggplot(df) +
  geom_density(aes(daynum_fill, group = trophic, fill = trophic), alpha = 0.5) +
  facet_wrap(~ metric)

p1 <- ggplot(df, aes(daynum_centered, group = trophic, fill = trophic)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ metric)

p2 <-ggplot(df, aes(y= daynum_centered, x = trophic, fill = trophic)) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~ metric)

p1 + p2  + plot_layout(guides = "collect") 

ggplot(df, aes(x = trophic, y = daynum_fill, group = trophic, fill = trophic)) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~ metric) +
  stat_compare_means(method = "anova")

p <- ggplot(df, aes(x = trophic, y = daynum_centered, group = trophic, fill = trophic)) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~ metric) 
add_pval(p, pairs = list(c(1, 2)), test='wilcox.test', alternative='two.sided')
