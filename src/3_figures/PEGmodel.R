


dat = read_csv('Data/analysis_ready/final_combined_dates_filled_v2.csv') |> 
  filter(!(lakeid == 'WI' & year < 2007))
unique(dat$metric)

# Which spring metrics
useorder = c("drsif_epiSpringMin","zoopDensity_spring","secchi_springmax")
lakeorder = c('BM','TR','CR','SP','CB','TB','ME','MO','AL','WI')

PEGyears = dat |> filter(lakeid %in% lakeorder) |> 
  filter(metric %in% useorder) |> 
  group_by(lakeid, year) |> 
  mutate(order = case_when(metric == 'drsif_epiSpringMin' ~ 1,
                           metric == 'zoopDensity_spring' ~ 2,
                           metric == 'secchi_springmax' ~ 3)) |> 
  arrange(order, .by_group = TRUE) |> 
  mutate(rank = data.table::frank(daynum, ties.method = 'average')) |> 
  mutate(use = case_when(first(rank) == 1 & last(rank) == 3 ~ TRUE,
                         first(rank) == 1.5 & last(rank) == 3 ~ TRUE,
                         first(rank) == 1 & last(rank) == 2.5 ~ TRUE,
                         TRUE ~ FALSE)) |> 
  slice(1) |> 
  dplyr::select(lakeid, year, use)

# Peg percent years 
PEGyears |> group_by(lakeid) |> 
  summarise(total = n(), PEG = sum(use)) |> 
  mutate(PEGper = 100*PEG/total)

p2 = dat |> filter(lakeid %in% lakeorder) |>
  left_join(PEGyears) |> 
  mutate(lakeid = factor(lakeid, levels = lakeorder)) |> 
  filter(metric %in% useorder) |> 
  mutate(metric = factor(metric, levels = useorder)) |> 
  ggplot() +
  geom_segment(data = . %>% filter(use == TRUE), aes(x = structure(-Inf, class = "Date"), 
                                                     xend = structure(Inf, class = "Date"), y = year, yend = year),
               color = 'grey80', size = 3) +
  geom_path(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), y = year, col = metric)) +
  geom_jitter(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), y = year, fill = metric), 
              shape = 21, stroke = 0.2,
              width = 0, height = 0.1) +
  scale_color_manual(values = c('#97bab7','#e0c3ba','#bf7058'), 
                     labels = c('Min Silica','Max Zooplankton', 'Max Secchi'), name = 'Spring Metrics') +
  scale_fill_manual(values = c('#97bab7','#e0c3ba','#bf7058'),
                    labels = c('Min Silica','Max Zooplankton', 'Max Secchi'), name = 'Spring Metrics') +
  scale_y_reverse(minor_breaks = seq(1995, 2018), breaks = seq(1985, 2015, by = 5), expand = c(0.01,0)) +
  scale_x_date(labels = date_format("%b"), minor_breaks = '1 month', breaks = '2 month') +
  facet_wrap(~lakeid, ncol = 10) +
  # xlab('Day of the Year') +
  theme_minimal(base_size = 9) +
  theme(axis.title = element_blank(),
        legend.justification = "left",
        legend.margin=margin(t = -0.2, unit='cm'),
        legend.position = 'bottom'); p2

ggsave('Figures_manuscript/PEGmodel.pdf',
       width=6, height=6, units="in", dpi=500, bg='white')

dat |> filter(metric == 'iceoff') |> 
  dplyr::select(lakeid, year, daynum) |> left_join(PEGyears) |> 
  filter(!is.na(use)) |> 
  ggplot() + 
  geom_histogram(aes(x = daynum, group = use, fill = use), binwidth = 5, alpha = 0.7) +
  facet_wrap(~lakeid)




