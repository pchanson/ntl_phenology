# script to visualize and compare different options for phenology metrics

library(tidyverse)
library(ggridges)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

comb_data = read_csv("../../Data/final_metric_data/final_combined_dates.csv")
comb_data$lakeid = factor(comb_data$lakeid, levels = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "FI", "ME", "MO", "WI"), ordered = TRUE)

# Metrics: c("anoxia", "anoxia_summer", "chlor_all", "chlor_fall", "chlor_spring", "daphnia_biomass", "doc", "energy", "iceoff", "iceon", "secchi_all", "secchi_openwater", "stability", "stratoff", "straton", "total_zoop_biomass"


# ==========================================
# anoxia
# ==========================================
comb_data %>% 
  filter(metric %in% c("anoxia", "anoxia_summer")) %>% 
  ggplot(aes(x=daynum, fill=metric)) +
  geom_histogram(position="identity", alpha=0.5, color="grey") + 
  # geom_density_ridges( stat = "binline", bins = 40, scale = 0.95, draw_baseline = FALSE) 
  theme_bw() + 
  facet_wrap(~lakeid) +
  theme(legend.position = c(0.87, 0.15))

# makes no or almost no difference for: BM, FI, ME, MO, TR, WI
# bogs have significant number early in year before stratification: CB, TB
# other northern lakes have more early in year but not as much as bogs: AL, CR, SP

# TAKEAWAY: probably go with anoxia_summer, but good discussion point to note lakes that have late winter anoxia (caused by volume and/or trophic status?)

# ==========================================
# chl
# ==========================================
comb_data %>% 
  filter(metric %in% c("chlor_all", "chlor_fall", "chlor_spring")) %>% 
  ggplot(aes(x=daynum, fill=metric)) +
  geom_histogram(position="identity", alpha=0.5, color="grey") + 
  # geom_density_ridges( stat = "binline", bins = 40, scale = 0.95, draw_baseline = FALSE) 
  theme_bw() + 
  facet_wrap(~lakeid) +
  theme(legend.position = c(0.87, 0.15))

# TODO: revisit notes on this; think we decided to go with using/comparing all three metrics? or maybe separate "all" from ("spring" and "fall")
# ==========================================
# secchi
# ==========================================
comb_data %>% 
  filter(metric %in% c("secchi_all", "secchi_openwater")) %>% 
  ggplot(aes(x=daynum, fill=metric)) +
  geom_histogram(position="identity", alpha=0.5, color="grey") + 
  # geom_density_ridges( stat = "binline", bins = 40, scale = 0.95, draw_baseline = FALSE) 
  theme_bw() + 
  facet_wrap(~lakeid) +
  theme(legend.position = c(0.87, 0.15))

# makes almost no difference in CB
# some difference in AL, CR, SP, TB, FI, ME, MO, WI (ME/MO are closer to almost no diff)
# big difference: BM, TR

# TAKEAWAY: most of the lakes have periods underice that are clearest of the year in some years
# QUESTION: do we want to limit all analyses to openwater? kind of limits our case for generic/inclusive phenology definition; but it also is a test of classical limnology "frameworks" (though maybe leaving underice measurements is a better test?); also we don't have very good under-ice sampling frequencies

# ==========================================
# daphnia / zoop biomass
# ==========================================
comb_data %>% 
  filter(metric %in% c("daphnia_biomass", "total_zoop_biomass")) %>% 
  ggplot(aes(x=daynum, fill=metric)) +
  geom_histogram(position="identity", alpha=0.5, color="grey") + 
  # geom_density_ridges( stat = "binline", bins = 40, scale = 0.95, draw_baseline = FALSE) 
  theme_bw() + 
  facet_wrap(~lakeid) +
  theme(legend.position = c(0.87, 0.15))

# lakes without much difference (daphnia likely dominates biomass | tightly coupled w/ other taxa): BM,ME, MO
# other lakes not clear that total or daphnia biomass gives a "better" distrib., except maybe CR, TB daphnia is better?

# TAKEAWAY: either probably okay, leaning towards going with daphnia
