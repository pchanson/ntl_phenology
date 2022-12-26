library(tidyverse)
library(lubridate)
library(ggcorrplot)
library(corrr)
library(patchwork)

# read in data
dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v2.csv")
dat$sampledate = as.Date(paste0(dat$year-1, "-12-31")) + dat$daynum_fill

dat2 = dat |> group_by(lakeid, metric) |> 
  summarise(day.mean = mean(daynum, na.rm = T), day.IQR = IQR(daynum, na.rm = T)) |> 
  ungroup() |> 
  mutate(lakeid = factor(lakeid, levels = c("AL", "BM", "CR", "SP", "TR", "TB", "CB", "FI", "ME", "MO", "WI"))) |> 
  mutate(metric = factor(metric, levels = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon"))) |> 
  filter(!is.na(metric))

ggplot(dat2) + 
  geom_hline(aes(yintercept = 28), linetype = 2) +
  geom_jitter(aes(x = metric, y = day.IQR, fill = lakeid), shape = 21, size = 3, width = 0.2) +
  scale_fill_manual(values = c("#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d", "#cc4c02", 
                               "#8c2d04", "#bae4b3", "#74c476", "#238b45","gold")) +
  ylab('IQR (days)') +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_blank()) 

ggsave('Figures/manuscript/phenologyIQR.png', width = 5, height = 3.5, dpi = 500)


# Select data
lakenames =  c("AL", "BM", "CR", "SP", "TR", "TB", "CB", "FI", "ME", "MO", "WI")
c.plot.list = list()
coff.df.list = list()
for (i in 1:length(lakenames)) {
  useVars = dat |> 
    filter(lakeid == lakenames[i]) |> 
    mutate(metric = factor(metric, levels = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", 
                                              "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", 
                                              "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon"))) |> 
    filter(!is.na(metric)) |> 
    select(year, metric, daynum) |> 
    arrange(metric) |> 
    pivot_wider(names_from = metric, values_from = daynum) |> 
    select(-year)
  
  ###### Compute a correlation matrix ######
  all_na <- function(x) all(is.na(x))
  useVars.na = useVars |>  mutate(
    # across(all_na(), ~replace_na(.x, 0))
    across(where(all_na), ~replace_na(.x, 0))
  )
  
  usecorr <- round(cor(useVars.na,use = "pairwise.complete.obs", method = 'pearson'), 2)
  # Compute a matrix of correlation p-values
  p.mat <- cor_pmat(useVars.na)
  
  # Melt
  usecorr <- reshape2::melt(usecorr, na.rm = FALSE, value.name = 'corr')
  p.mat <- reshape2::melt(p.mat, na.rm = FALSE, value.name = 'p')
  
  coff.df = usecorr |> as_tibble() |> 
    left_join(p.mat) |> 
    mutate(corr.p = if_else(p < 0.05, corr, NA_real_)) |> 
    mutate(lakeid = lakenames[i])

  coff.df.list[[i]] = coff.df
  
  # # Correlation plot at p < 0.05 sig
  # c.plot = ggcorrplotHD(usecorr, type = "full", hc.order = F,
  #                     lab = TRUE, p.mat = p.mat, insig = "blank",
  #                     outline.col = "white", tl.cex = 8, lab_size = 2,
  #                     ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.05,
  #                     colors = c("#E46726", "grey95", "#6D9EC1")) +
  #   labs(title = lakenames[i])
  # c.plot.list[[i]] = c.plot

  
}

# wrap_plots(c.plot.list)

# Bind list
coff.df = do.call(rbind.data.frame, coff.df.list) |> 
  mutate(lakeid = factor(lakeid, levels = c("AL", "BM", "CR", "SP", "TR", "TB", "CB", "FI", "ME", "MO", "WI"))) |> 
  arrange(Var1) |> 
  filter(!is.na(corr.p)) |> 
  filter(corr.p > 0 & corr.p < 1)

# Pot correlations 
p1 = ggplot(data = coff.df, mapping = aes(x = Var1, y = Var2, fill = lakeid)) +
  geom_jitter(shape = 21, size = 2.5, width = 0.15, height = 0.15, alpha = 0.8, stroke = 0.2) +
  # geom_tile(color = 'gray') +
  scale_fill_manual(values = c("#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d", "#cc4c02", 
                               "#8c2d04", "#bae4b3", "#74c476", "#238b45","gold")) +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title = element_blank(), 
        legend.key.height = unit(0.5, 'cm')) +
  labs(caption = 'Figure X: Phenological metrics that have positive correlations (p > 0.05) at individual lakes.')

ggsave(plot = p1, 'Figures/manuscript/phenologyCorr.png', width = 5, height = 3.5, dpi = 500)


