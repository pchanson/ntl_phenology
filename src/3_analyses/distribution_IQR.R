# script to calculate IQR for each lake/event 

dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v2.csv")
dat$sampledate = as.Date(paste0(dat$year-1, "-12-31")) + dat$daynum_fill

vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")
vars_label = c("ice off", "strat onset", "spring bloom", "clearwater", "daphnia", "DOC", "TP hypo min", "TP epi max",  "anoxia",  "stability", "energy", "TP epi min", "TP hypo max", "strat offset", "ice on")

lakeColors_df = data.frame(
  Lake = c("AL", "BM", "CR", "SP", "TR", "CB", "TB", "ME", "MO", "FI"),
  Color = c("#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d", "#cc4c02", "#8c2d04", "#bae4b3", "#74c476", "#238b45")
)
Lake_colors = str_to_upper(lakeColors_df$Color)
names(Lake_colors) = lakeColors_df$Lake

dat %>% 
  filter(metric %in% vars_order & lakeid != "WI") %>% 
  group_by(lakeid, metric) %>% 
  summarise(fQ = quantile(daynum_fill, 0.25, na.rm=T),
            tQ = quantile(daynum_fill, 0.75, na.rm=T),
            IQR = tQ - fQ) %>% 
  mutate(lakeid = factor(lakeid, levels = c("AL", "BM", "CR", "SP", "TR", "CB", "TB", "ME", "MO", "FI", "WI"), ordered=T),
    metric = factor(metric, levels=vars_order, ordered = T)) %>% 
  ggplot(aes(x=lakeid, y=IQR, fill=lakeid)) +
  geom_bar(stat="identity") +
  facet_wrap(~metric, scales="free_y") +
  scale_fill_manual(values=Lake_colors)  +
  labs(title="Interquartile range of event timing", y="IQR (days)") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  
