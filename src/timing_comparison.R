library(tidyverse)
library(ggridges)

# make file combining all variables
phen_dates = read_csv("../Data/phenology_dates_v1.csv")

phen_dates_wide = pivot_wider(phen_dates, id_cols = c("id", "decade", "year"), names_from = "variable", values_from = "value")

var_order = c("iceoff", "straton", "clearwater", "daphnia", "chla", "doc", "anoxia", "stability", "energy", "stratoff", "iceon") # need to add doc

all_new_cols = c()
for(i in 1:(length(var_order) - 1)){
  newcol = paste(var_order[i], var_order[i+1], sep=":")
  all_new_cols = c(all_new_cols, newcol)
  phen_dates_wide[, newcol] = pull(phen_dates_wide[, var_order[i+1]]) - pull(phen_dates_wide[, var_order[i]])
}

plot_df = phen_dates_wide %>% 
  select(id, year, all_of(all_new_cols)) %>% 
  pivot_longer(cols = all_new_cols) %>% 
  mutate(name = factor(name, levels = all_new_cols))

plot_df %>% 
  # filter(!is.na(id)) %>%
  ggplot() +
  geom_histogram(aes(x=value, fill=id)) + 
  facet_grid(rows=vars(id), cols=vars(name), scales = "free_x") + 
  geom_vline(xintercept = 0)

g = plot_df %>% 
  # filter(!is.na(id)) %>%
  ggplot() +
  geom_density_ridges(aes(x=value, y=id, fill=id)) + 
  facet_grid(cols=vars(name)) + 
  geom_vline(xintercept = 0) + 
  ggtitle("A:B = days between A and B") +
  xlab("Days between A and B") +
  theme_minimal()  

ggsave(file = '../Figures/within_year_phenology.png', g, dpi = 500, width =9, height = 8)
ggsave(file = '../Figures/within_year_phenology.pdf', g, width =9, height = 8)

g2 = 
plot_df %>% 
  filter(id %in% c("ME", "BM", "CB", "AL")) %>%
  ggplot() +
  geom_density_ridges(aes(x=value, y=id, fill=id)) + 
  facet_grid(cols=vars(name)) + 
  geom_vline(xintercept = 0) + 
  ggtitle("A:B = days between A and B") +
  xlab("Days between A and B") +
  theme_minimal() +
  lims(x=c(-365, 365)) +
  theme(axis.text = element_text(size=4))

# construct matrix
all_var_combos = expand.grid(var_order, var_order)
all_lake_years = unique(phen_dates_wide[, c("id", "year")])

all_lyv = expand_grid(all_var_combos, all_lake_years)

all_lyv_1 = phen_dates %>% 
  select(id, year, variable, value) %>% 
  rename(Var1 = variable) %>% 
  right_join(all_lyv) %>% 
  rename(Value1 = value)

all_lyv_2 = phen_dates %>% 
  select(id, year, variable, value) %>% 
  rename(Var2 = variable) %>% 
  right_join(all_lyv_1) %>% 
  rename(Value2 = value) %>% 
  select(id, year, Var1, Value1, Var2, Value2) %>% 
  mutate(doy_diff = Value2 - Value1)

write_csv(all_lyv_2, "../Data/all_event_timing_differences.csv")
