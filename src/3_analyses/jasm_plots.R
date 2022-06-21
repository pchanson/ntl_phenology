# (run start of analyses_update.Rmd first)

lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "FI", "ME", "MO", "WI")

vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc",  "anoxia_summer", "stability", "energy", "stratoff", "iceon")

dat %>% 
  filter(lakeid == "ME") %>% 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(c("ice off", "strat onset", "spring bloom", "clearwater", "daphnia", "DOC",  "anoxia", "stability", "energy", "strat offset", "ice on")))) %>% 
  ggplot() + 
  stat_density_ridges(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                          y= metric, col = metric, fill = metric), 
                      alpha = 0.5, quantile_lines = T, quantiles = 2, size=1) +
  scale_fill_manual(values=met.brewer("Archambault", 11)) + 
  scale_color_manual(values=met.brewer("Archambault", 11)) +
  scale_x_date(labels = date_format("%b")) +
  # facet_wrap(~ (lakeid)) +
  xlab('') + ylab('')+ ggtitle("Lake Mendota") +
  theme_minimal(base_size = 9) + 
  theme(axis.text = element_text(size=16),
        plot.title=element_text(size=18)) +
  guides(fill="none", color="none")


# add and extra lake in N
lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "", "FI", "ME", "MO", "WI")

# vars_order = c("iceoff", "straton", "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc", "chlor_all", "anoxia_summer", "stability", "energy", "chlor_fall", "stratoff", "iceon")
vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc",  "anoxia_summer", "stability", "energy", "stratoff", "iceon")

empty_lake = dat %>% 
  select(metric) %>% 
  mutate(lakeid = "") %>% 
  distinct()

dat %>% 
  full_join(empty_lake) %>% 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(c("ice off", "strat onset", "spring bloom", "clearwater", "daphnia", "DOC",  "anoxia", "stability", "energy", "strat offset", "ice on")))) %>% 
  ggplot() + 
  stat_density_ridges(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                          y= metric, col = metric, fill = metric), 
                      alpha = 0.5, quantile_lines = T, quantiles = 2) +
  scale_fill_manual(values=met.brewer("Archambault", 11)) + 
  scale_color_manual(values=met.brewer("Archambault", 12)) +
  scale_x_date(labels = date_format("%b")) +
  facet_wrap(~ (lakeid)) +
  xlab('') + ylab('')+
  theme_minimal(base_size = 9) + 
  theme(axis.text = element_text(size=12),
        strip.text=element_text(size=14),
        panel.spacing.y=unit(0.5,"lines")) +
  guides(fill="none", color="none")
