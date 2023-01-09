# This code uses the tidyverse, lubridate, ggcorrplot, and corrr packages to
# analyze phenology data among 11 lakes. First, the variables are labeled and
# ordered, and the required data is read from a ".csv" file. The code then
# creates a plot of the Interquartile Ranges (IQR) of the phenology data for the
# 11 lakes, and saves the plot as a ".png" file. Next, the code creates a
# correlation matrix of the phenology data for the 11 lakes. The correlation
# matrix is melted and stored as a dataframe. This dataframe is then used to
# create a plot of the correlation of the phenology data for the 11 lakes, and
# the plot is saved as a ".png" file.

figure3 <- function(path_in, path_out) {
  
  vars_order = c("iceoff", "straton", "secchi_max", "secchi_min", "zoopDensity", "doc_epiMax", 
                 "drsif_epiMin", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", 
                 "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")
  vars_label = c("ice off", "strat onset", "SecchiMax","SecchiMin", "zoopDensity", "DOC max", 
                 "Si Min", "TP hypo min", "TP epi max",  "anoxia",  "stability", "energy", 
                 "TP epi min", "TP hypo max", "strat offset", "ice on")
  
  # read in data
  dat = read_csv(path_in)
  
  dat2 = dat |> group_by(lakeid, metric) |> 
    summarise(day.mean = mean(daynum, na.rm = T), day.IQR = IQR(daynum, na.rm = T)) |> 
    ungroup() |> 
    mutate(lakeid = factor(lakeid, levels = c("AL", "BM", "CR", "SP", "TR", "CB", "TB", "FI", "ME", "MO", "WI"))) |> 
    mutate(metric = factor(metric, levels = vars_order)) |> 
    filter(!is.na(metric))
  
  plot.IQR = ggplot(dat2) + 
    geom_hline(aes(yintercept = 28), linetype = 2) +
    geom_jitter(aes(x = metric, y = day.IQR, fill = lakeid), shape = 21, size = 2.5, width = 0.2, height = 0, stroke = 0.2) +
    scale_fill_manual(values = c("#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d", "#cc4c02", 
                                 "#8c2d04", "#bae4b3", "#74c476", "#238b45","gold")) +
    ylab('IQR (days)') +
    scale_x_discrete(breaks = vars_order, labels = vars_label) +
    theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          axis.title.x = element_blank(), 
          legend.key.height = unit(0.3,'cm'),
          legend.title = element_blank()); plot.IQR
  
  # ggsave(plot = plot.IQR, 'Figures/manuscript/phenologyIQR.png', width = 5, height = 3.5, dpi = 500)
  
  ################################ Correlation ################################
  # Select data
  lakenames =  c("AL", "BM", "CR", "SP", "TR", "TB", "CB", "FI", "ME", "MO", "WI")
  c.plot.list = list()
  coff.df.list = list()
  for (i in 1:length(lakenames)) {
    useVars = dat |> 
      filter(lakeid == lakenames[i]) |> 
      mutate(metric = factor(metric, levels = vars_order)) |> 
      filter(!is.na(metric)) |> 
      dplyr::select(year, metric, daynum) |> 
      arrange(metric) |> 
      pivot_wider(names_from = metric, values_from = daynum) |> 
      dplyr::select(-year)
    
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
    # |> mutate(p = p.adjust(p, method = "holm"))
    
    coff.df = usecorr |> as_tibble() |> 
      left_join(p.mat) |> 
      mutate(corr.p = if_else(p < 0.01, corr, NA_real_)) |> 
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
    mutate(lakeid = factor(lakeid, levels = c("AL", "BM", "CR", "SP", "TR", "CB", "TB", "FI", "ME", "MO", "WI"))) |> 
    arrange(Var1) |> 
    filter(!is.na(corr.p)) |> 
    filter(corr.p > 0 & corr.p < 1) |> 
    # need a row with ice on so axes match up in figure 
    bind_rows(data.frame(Var1 = factor('iceon'), Var2 = factor('iceon'), 
                         corr = NA, p = NA, corr.p = NA, lakeid = NA))
  
  # Pot correlations 
  plot.Cor = ggplot(data = coff.df, mapping = aes(x = Var1, y = Var2, fill = lakeid, color = lakeid)) +
    geom_jitter(shape = 21, size = 2.5, width = 0.15, height = 0.15, alpha = 0.8, stroke = 0.2) +
    # geom_tile(color = 'gray') +
    scale_fill_manual(values = c("#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d", "#cc4c02", 
                                 "#8c2d04", #"#bae4b3", 
                                 "#74c476", "#238b45","gold"), na.translate = F) +
    scale_color_manual(values = rep('black', 10), na.translate = F) +
    scale_x_discrete(breaks = vars_order, labels = vars_label) +
    scale_y_discrete(breaks = vars_order, labels = vars_label) +
    theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          axis.title = element_blank(), 
          legend.key.height = unit(0.3, 'cm'),
          legend.title = element_blank()); plot.Cor
  
  # ggsave(plot = plot.Cor, 'Figures/manuscript/phenologyCorr.png', width = 5, height = 3.5, dpi = 500)
  
  
  ################################ Join ################################
  plot.IQR/plot.Cor + 
    plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
    theme(plot.tag = element_text(size  = 8))
  
  ggsave(path_out, width = 6, 
         height = 4.5, dpi = 500)

}
# coff.table = coff.df |> 
#   dplyr::select(lakeid, Var1, Var2, corr, p) |> 
#   arrange(lakeid)
  
# print(xtable(coff.table), include.rownames = FALSE)
