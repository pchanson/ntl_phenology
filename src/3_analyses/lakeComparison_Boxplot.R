library(tidyverse)
library(patchwork)
library(scales)

# read in data
dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v2.csv")

# Fig 1: Ridges
vars_order = c("iceoff", "straton", "secchi_max", "secchi_min", "zoopDensity_CC", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")
vars_label = c("ice off", "strat onset", "SecchiMax","SecchiMin", "zoopDensity", "DOC", "TP hypo min", "TP epi max",  "anoxia",  "stability", "energy", "TP epi min", "TP hypo max", "strat offset", "ice on")

# add and extra lake in N
lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "", "FI", "ME", "MO", "WI")

##### Functions #####
all_na <- function(x) all(is.na(x))

corrFunction <- function(x,y) {
  round(cor(x, y, use = "pairwise.complete.obs", method = 'pearson'), 2)
}
corrPFunction <- function(x,y) {
  round(cor.test(x, y, method = 'pearson')$p.value, 3)
}

customTheme <- theme_bw(base_size = 8)  +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(), 
        axis.title = element_blank(),
        legend.key.width = unit(0.2, 'cm'),
        legend.key.height = unit(0.2, 'cm'),
        legend.margin=margin(t = 0, unit='cm'),
        plot.title = element_text(size = 8))

################################ ME and MO ################################
df1 = dat %>% 
  filter(lakeid %in% c('ME','MO')) |> 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) 
  
df1.Cor = df1 |> 
  select(lakeid, year, metric, daynum) |> 
  pivot_wider(names_from = lakeid, values_from = daynum) |> 
  mutate(across(where(all_na), ~replace_na(.x, 0))) |> 
  group_by(metric) |> 
  summarise(cor = corrFunction(ME, MO), corP = corrPFunction(ME,MO))

  
p1 = ggplot(df1) + 
  geom_boxplot(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                          y = metric, color = lakeid, fill = lakeid), 
                      alpha = 0.5, position = position_dodge(0.2), linewidth = 0.2, outlier.size = 1, outlier.stroke = 0.2) +
  geom_point(data = df1.Cor |> filter(corP <= 0.05), aes(x =  as.Date('2019-03-01'), y = metric), size = 1, shape = 8) +
  scale_fill_manual(values = c("#74c476", "#238b45")) + 
  scale_color_manual(values = c("#74c476", "#238b45")) + 
  scale_x_date(labels = date_format("%b"), breaks = '3 months', minor_breaks = '1 month') +
  labs(title = 'c) Eutrophic') +
  customTheme


################################ CB and TB ################################
df1 = dat %>% 
  filter(lakeid %in% c('CB','TB')) |> 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) 

df1.Cor = df1 |> 
  select(lakeid, year, metric, daynum) |> 
  pivot_wider(names_from = lakeid, values_from = daynum) |> 
  mutate(across(where(all_na), ~replace_na(.x, 0))) |> 
  group_by(metric) |> 
  summarise(cor = corrFunction(CB, TB), corP = corrPFunction(CB, TB))


p2 = ggplot(df1) + 
  geom_boxplot(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                   y = metric, color = lakeid, fill = lakeid), 
               alpha = 0.5, position = position_dodge(0.2), linewidth = 0.2, outlier.size = 1, outlier.stroke = 0.2) +
  geom_point(data = df1.Cor |> filter(corP <= 0.05), aes(x =  as.Date('2019-03-01'), y = metric), size = 1, shape = 8) +
  scale_fill_manual(values = c("#cc4c02","#8c2d04")) + 
  scale_color_manual(values = c("#cc4c02","#8c2d04")) + 
  scale_x_date(labels = date_format("%b"), breaks = '3 months', minor_breaks = '1 month') +
  labs(title = 'b) Dystrophic') +
  customTheme

################################ CR and SP ################################
df1 = dat %>% 
  filter(lakeid %in% c('CR','SP')) |> 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) 

df1.Cor = df1 |> 
  select(lakeid, year, metric, daynum) |> 
  pivot_wider(names_from = lakeid, values_from = daynum) |> 
  mutate(across(where(all_na), ~replace_na(.x, 0))) |> 
  group_by(metric) |> 
  summarise(cor = corrFunction(CR, SP), corP = corrPFunction(CR, SP))


p3 = ggplot(df1) + 
  geom_boxplot(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                   y = metric, color = lakeid, fill = lakeid), 
               alpha = 0.5, position = position_dodge(0.2), linewidth = 0.2, outlier.size = 1, outlier.stroke = 0.2) +
  geom_point(data = df1.Cor |> filter(corP <= 0.05), aes(x =  as.Date('2019-03-01'), y = metric), size = 1, shape = 8) +
  scale_fill_manual(values = c("#74a9cf", "#2b8cbe")) + 
  scale_color_manual(values = c("#74a9cf", "#2b8cbe")) + 
  scale_x_date(labels = date_format("%b"), breaks = '3 months', minor_breaks = '1 month') +
  labs(title = 'a) Oligotrophic') +
  customTheme

################################ JOIN ################################
p3 + p2 + p1 

ggsave('Figures/manuscript/lakeComparison_Boxplot.png', width = 5, height = 3.5, dpi = 500)
