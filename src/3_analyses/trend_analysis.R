library(tidyverse)
library(broom)
library(wql)

dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v2.csv")

vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")

phys_vars = c("iceoff", "straton", "stability", "energy", "stratoff", "iceon")
cb_vars = vars_order[!vars_order %in% phys_vars]

met_cats = dat %>% 
  select(metric) %>% 
  distinct() %>% 
  mutate(metric_category = ifelse(metric %in% phys_vars, "physical", "biological"))

lake_cats = dat %>% 
  select(lakeid) %>% 
  distinct() %>% 
  mutate(lake_category = ifelse(lakeid %in% c("FI", "WI", "ME", "MO"), "South", "North"))

# simple linear trend
lm_slopes = dat %>% 
  filter(!is.na(daynum_fill)) %>% 
  group_by(lakeid, metric) %>% 
  do(tidy(lm(daynum_fill ~ year, data = .))) %>% 
  filter(term == "year" & metric %in% vars_order) 

lm_slopes = lm_slopes %>% 
  left_join(lake_cats) %>% 
  left_join(met_cats)

lm_slopes %>% 
  # filter(p.value < 0.1) %>% 
  ggplot(aes(x=metric, y=estimate, color=lake_category)) +
  geom_boxplot()+
  # facet_wrap(~metric) +
  theme_bw() + 
  geom_hline(yintercept = 0)

lm_slopes %>% 
  mutate(estimate = ifelse(p.value < 0.05, estimate, NA)) %>% 
  mutate(lakeid = factor(lakeid, 
                         levels = c("AL", "BM", "CB", "CR", "SP", "SR", "TB", "TR", "FI", "ME", "MO", "WI"),
                         ordered = T)) %>% 
  mutate(metric = factor(metric, levels = c(phys_vars, cb_vars), ordered=T)) %>% 
  ggplot(aes(x=metric, y=lakeid, fill=estimate)) +
  geom_tile(color="white") + 
  scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
                             '#d1e5f0','#67a9cf','#2166ac'),
                    n.breaks=9, limits=c(-lmt,lmt), na.value = "grey") +
  geom_hline(yintercept = 7.5) +
  geom_vline(xintercept = 6.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x="", title = "lm() sig. trends (p < 0.05)", fill="lm slope\n days/year")

# try with Mann-Kendall
mannken_sen_slope = dat %>% 
  select(lakeid, metric) %>% 
  distinct() %>% 
  filter(metric %in% vars_order) %>% 
  mutate(sen.slope = NA, p.value=NA)

for(i in 1:nrow(mannken_sen_slope)){
  cur_dat = dat %>% 
    filter(lakeid == mannken_sen_slope[[i, "lakeid"]] & metric == mannken_sen_slope[[i, "metric"]]) %>% 
    arrange(year) %>% 
    pull(daynum_fill)
    hold = mannKen(cur_dat)
  
  mannken_sen_slope[[i, "sen.slope"]] = hold[["sen.slope"]]  
  mannken_sen_slope[[i, "p.value"]] = hold[["p.value"]]  
  
}

mkss_sig = mannken_sen_slope %>% 
  mutate(sen.slope = ifelse(p.value < 0.05, sen.slope, NA))

lmt = max(abs(mkss_sig$sen.slope), na.rm=T)

mkss_sig %>% 
  mutate(lakeid = factor(lakeid, 
                         levels = c("AL", "BM", "CB", "CR", "SP", "SR", "TB", "TR", "FI", "ME", "MO", "WI"),
                         ordered = T)) %>% 
  mutate(metric = factor(metric, levels = c(phys_vars, cb_vars), ordered=T)) %>% 
  ggplot(aes(x=metric, y=lakeid, fill=sen.slope)) +
  geom_tile(color="white") + 
  scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
                             '#d1e5f0','#67a9cf','#2166ac'),
                    n.breaks=9, limits=c(-lmt,lmt), na.value = "grey") +
  geom_hline(yintercept = 7.5) +
  geom_vline(xintercept = 6.5) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(x="", title = "Mann-Kendall sig. trends (p < 0.05)", fill="Sen Slope\n days/year")

# plot the significant trends
vars_label = c("ice off", "strat onset", "spring bloom", "clearwater", "daphnia", "DOC", "TP hypo min", "TP epi max",  "anoxia",  "stability", "energy", "TP epi min", "TP hypo max", "strat offset", "ice on")

mkss_sig %>% 
  filter(p.value < 0.05) %>% 
  left_join(dat) %>% 
  mutate( sen_slope = paste("Sen Slope =", round(sen.slope, 1), "d/yr"),
         metric = factor(metric, levels=vars_order, labels = vars_label),
         lake_metric = paste(lakeid, metric, sep=" : ")) %>% 
  ggplot(aes(x=year, y=daynum_fill)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~lake_metric+sen_slope, scale="free_y") +
  geom_smooth()
  



  