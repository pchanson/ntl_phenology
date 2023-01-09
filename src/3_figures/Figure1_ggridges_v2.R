dat = read_csv(path_in)
# dat$sampledate = as.Date(paste0(dat$year-1, "-12-31")) + dat$daynum_fill

vars_order = c("", "iceoff", "straton", "secchi_max", "secchi_min", "zoopDensity", "doc_epiMax", 
               "drsif_epiMin", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", 
               "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")
vars_label = c("","ice off", "strat onset", "SecchiMax","SecchiMin", "zoopDensity", "DOC max", 
               "Si Min", "TP hypo min", "TP epi max",  "anoxia",  "stability", "energy", 
               "TP epi min", "TP hypo max", "strat offset", "ice on")

lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "", "FI", "ME", "MO", "WI")

dat.iqr = dat |> group_by(lakeid, metric) |> 
  summarise(day.mean = mean(daynum, na.rm = T), day.IQR = IQR(daynum, na.rm = T)) |> 
  ungroup() |> 
  mutate(lakeid = factor(lakeid, levels = lakes_order)) |> 
  mutate(metric = factor(metric, levels = vars_order)) |> 
  filter(!is.na(metric))

plotridge <- function(uselakes) {
  dat %>% 
    filter(lakeid %in% uselakes) |> 
    filter(metric %in% vars_order) %>% 
    mutate(lakeid = factor(lakeid, levels = lakes_order),
           metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) %>% 
    ggplot() + 
    stat_density_ridges(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                            y= metric, col = metric, fill = metric), 
                        alpha = 0.5, quantile_lines = T, quantiles = 2, size = 0.3) +
    scale_fill_manual(values=met.brewer("Archambault", length(vars_order))) + 
    scale_color_manual(values=met.brewer("Archambault", length(vars_order))) +
    scale_x_date(labels = date_format("%b")) +
    facet_wrap(~lakeid, nrow = 1, strip.position = "top") +
    theme_minimal(base_size = 8) + 
    theme(axis.text.y = element_text(size=6),
          axis.title = element_blank(), 
          strip.text = element_text(size=10),
          panel.spacing.y = unit(0.4,"lines"),
          strip.background = element_blank(),
          panel.grid.major = element_line(size = 0.2)) +
    guides(fill="none", color="none")
}


plotiqr <- function(uselakes) {
  empty_metric = dat.iqr %>% 
    select(lakeid) %>% 
    mutate(metric = "") %>% 
    distinct()
  
  dat.iqr |> full_join(empty_metric) |>   
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) |> 
    filter(lakeid %in% uselakes) |> 

  ggplot() + 
    geom_vline(aes(xintercept = 28), linetype = 2) +
    geom_jitter(aes(y = metric, x = day.IQR, fill = metric), shape = 21, size = 1.5, width = 0.2, height = 0, stroke = 0.2) +
    xlab('IQR (days)') +
    scale_fill_manual(values=met.brewer("Archambault", length(vars_order))) + 
    scale_color_manual(values=met.brewer("Archambault", length(vars_order))) +
    theme_minimal(base_size = 8) +
    # labs(title = 'IQR (days)') +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          legend.position = 'none',
          legend.key.height = unit(0.3,'cm'),
          legend.title = element_blank(),
          panel.grid.major = element_line(size = 0.2)) 

}



p1 = plotridge(uselakes = c("BM", "TR"))
p1.5 = plotridge(uselakes = c("CR", "SP"))
p2 = plotridge(uselakes = c("CB","TB"))
p3 = plotridge(uselakes = c("AL", "WI"))
p4 = plotridge(uselakes = c("ME", "MO"))

p11 = plotiqr(uselakes = c("BM", "CR", "SP", "TR"))
p21 = plotiqr(uselakes = c("CB","TB"))
p31 = plotiqr(uselakes = c("AL", "WI"))
p41 = plotiqr(uselakes = c("ME", "MO"))

layout <- '
AAF
BB#
CCG
DDH
EEI
'

p1 + p1.5 + p2 + p3 + p4 + p11 + p21 + p31 + p41 +
  plot_layout(design = layout)

ggsave('Figures_manuscript/Figure1_v2.png',
       width=6, height=8, units="in", dpi=500, bg="white")

