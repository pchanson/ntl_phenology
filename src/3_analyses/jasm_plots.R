# (run start of analyses_update.Rmd first)

lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "FI", "ME", "MO", "WI")

vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc",  "anoxia_summer", "stability", "energy", "stratoff", "iceon")

dat %>% 
  filter(lakeid == "ME") %>% 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(c("iceoff", "straton", "spring bloom", "clearwater", "daphnia", "doc",  "anoxia summer", "stability", "energy", "stratoff", "iceon")))) %>% 
  ggplot() + 
  stat_density_ridges(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                          y= metric, col = metric, fill = metric), 
                      alpha = 0.5, quantile_lines = T, quantiles = 2) +
  scale_fill_manual(values=met.brewer("Archambault", 11)) + 
  scale_color_manual(values=met.brewer("Archambault", 11)) +
  scale_x_date(labels = date_format("%b")) +
  # facet_wrap(~ (lakeid)) +
  xlab('') + ylab('Density')+ ggtitle("Lake Mendota") +
  theme_minimal(base_size = 9) + 
  theme(axis.text = element_text(size=12),
        plot.title=element_text(size=14),
        legend.key.height = unit(0.5,"cm"), 
        legend.key.width = unit(0.3,"cm"))
