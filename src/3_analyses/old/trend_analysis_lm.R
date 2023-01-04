library(broom)
library(tidyverse)
library(purrr)

# read in data
dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v2.csv")

nested = dat %>% 
  filter(!is.na(daynum)) %>%
  nest(data = -c(lakeid, metric)) %>% 
  dplyr::mutate(
    fit = purrr::map(data, ~ lm(daynum ~ year, data = .x)),
    tidied = purrr::map(fit, tidy)) %>% 
  unnest(tidied)

trend = nested |> 
  filter(term == 'year') |> 
  filter(p.value < 0.05 & estimate >= 0)

plots.out = list()
for (i in 1:nrow(trend)) {
  plots.out[[i]] = ggplot(dat |> filter(lakeid == trend$lakeid[i] & metric == trend$metric[i])) +
    geom_smooth(aes(x = year, y = daynum), method = 'lm', color = 'black') +
    geom_point(aes(x = year, y = daynum), size = 1) +
    ylab('Day of the year') + ylim(140,300) +
    labs(title = paste0(trend$lakeid[i],', metric = ',trend$metric[i])) +
    theme_bw(base_size = 8) +
    theme(axis.title.x = element_blank())
}

patchwork::wrap_plots(plots.out)

ggsave('Figures/')
