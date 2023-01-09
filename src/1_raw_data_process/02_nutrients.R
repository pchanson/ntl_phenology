
nutrients <- function(path_in, physics_file, ice_file, path_out) {
  # Updated 2022-12-28
  varsWant = c("doc_epiMax", "totpuf_epiMin", #"totpuf_epiMax", 
               "totpuf_hypoMax", #"totpuf_hypoMin", 
               "drsif_epiMin", 
               "totnuf_epiMin", "totnuf_hypoMax")
  
  #################### FUNCTIONS ####################
  # filtering function - turns outliers into NAs to be removed
  filter_lims <- function(x){
    # l = quantile(x, probs = 0.001)
    # u = quantile(x, probs = 0.999)
    stdx = sd(x)
    
    u = mean(x, na.rm = T) + 3*stdx
    l = mean(x, na.rm = T) - 3*stdx
    for (i in 1:length(x)){
      x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
    }
    return(x)
  }
  
  #################### LOAD DATA ####################
  inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/57/802d63a4c35050b09ef6d1e7da3efd3f"
  infile1 <- tempfile()
  download.file(inUrl1, infile1, method = "curl")
  LTERnutrients <- read_csv(infile1)
  
  # removed flagged data
  lternuts.flagged = LTERnutrients %>%
    mutate(across(everything(), ~replace(., .<0 , NA))) %>%
    rename_all( ~ str_replace(., "_sloh", '.sloh')) %>%
    rename_all( ~ str_replace(., "_n", '.n')) %>%
    rename_at(vars(ph:drsif.sloh), ~ str_c("value_",.)) %>%
    rename_at(vars(flagdepth:flagdrsif.sloh), ~ str_c("error_",.)) %>%
    rename_all(~str_replace_all(.,"flag","")) %>%
    pivot_longer(-(lakeid:event), names_to = c('.value','item'), names_sep = '_') %>%
    filter(!is.na(value) & value>= 0) %>%
    filter(!str_detect(error,'A|K|L|H|Q') | is.na(error)) %>%
    select(-error) %>% 
    mutate(value = case_when(str_detect(item, ".sloh") ~ value*1000, #change sloh from mg to µg
                             TRUE ~ value)) %>% 
    mutate(item = case_when(str_detect(item, ".sloh") ~  str_remove(item, ".sloh"),
                            TRUE ~ item))
  
  # Exclude outliers based on statistics. Remove < 5th and > 95th percentile
  lternuts.flagged = lternuts.flagged |> 
    mutate(value = filter_lims(value))
  
  # get ice on/off dates
  ice0 = read_csv(ice_file) |> 
    filter(metric == 'iceoff') |> 
    select(lakeid, year4 = year, lastice = sampledate)
  
  # Which depths to use? 
  maxDepths = lternuts.flagged |> 
    group_by(lakeid, depth) %>% tally() %>% 
    filter(if_else(lakeid %in% c('ME',"MO","WI","FI"), n >= 2500, n>= 4000)) %>% 
    group_by(lakeid) |> 
    filter(depth == max(depth)) |> 
    rename(maxDepth = depth)
  
  
  #################### MANIPULATE DATA ####################
  
  # Load stratification dates
  strat = read_csv(physics_file) %>% 
    filter(metric %in% c('straton', 'stratoff')) %>% 
    select(-daynum) %>% 
    pivot_wider(names_from = "metric", values_from = "sampledate") %>% 
    rename(year4 = year)
  
  # restrict to epi/hypo and stratification period
  nuts = lternuts.flagged %>% 
    # left_join(strat, by = c("lakeid", "year4")) %>% 
    # filter(sampledate >= straton & daynum <= stratoff) %>% #filter to during strat
    left_join(ice0) |> 
    filter(sampledate > lastice) |> 
    left_join(maxDepths) |> 
    filter(depth == 0 | depth == maxDepth) 

  # Take mean of any duplicates
  nuts.mean = nuts |> 
    group_by(lakeid, item, sampledate, year4, depth) |> 
    summarise(value = mean(value, na.rm = T))

  # find day min or max values
  epi_min = nuts.mean %>% 
    filter(depth == 0) |> 
    group_by(lakeid, year4, item) %>% 
    slice_min(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(daynum = yday(sampledate), metric = paste0(item, "_epiMin")) %>% 
    ungroup() %>% 
    select(lakeid, metric, sampledate, year4, daynum)
  
  epi_max = nuts.mean %>% 
    filter(depth == 0) |> 
    group_by(lakeid, year4, item) %>% 
    slice_max(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(daynum = yday(sampledate), metric = paste0(item, "_epiMax")) %>% 
    ungroup() %>% 
    select(lakeid, metric, sampledate, year4, daynum)
  
  hypo_min = nuts.mean %>% 
    filter(depth != 0) |> 
    group_by(lakeid, year4, item) %>% 
    slice_min(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(daynum = yday(sampledate), metric = paste0(item, "_hypoMin")) %>% 
    ungroup() %>% 
    select(lakeid, metric, sampledate, year4, daynum)
  
  hypo_max = nuts.mean %>% 
    filter(depth != 0) |> 
    group_by(lakeid, year4, item) %>% 
    slice_max(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(daynum = yday(sampledate), metric = paste0(item, "_hypoMax")) %>% 
    ungroup() %>% 
    select(lakeid, metric, sampledate, year4, daynum)
  
  comb = bind_rows(epi_min, epi_max, hypo_min, hypo_max) |> 
    rename(year = year4)
  
  # Plot density distributions
  # ggplot(comb) +
  #   geom_density(aes(x = daynum)) +
  #   facet_grid(rows=vars(lakeid), cols=vars(metric))
  
  write_csv(comb %>% filter(metric %in% varsWant),file = path_out)
  
  return(path_out) 
}
