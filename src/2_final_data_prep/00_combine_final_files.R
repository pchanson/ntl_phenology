######################################################################
############## Read in and combine final phenology dates ##############
######################################################################

combine_final_files <- function(ice_file, physics_file, nutrients_file, secchi_file, zoopDensity_file, path_out) {

  # files_path = "Data/final_metric_files"
  # files0 = list.files(files_path, full.names = T)
  # files = files0[files0 != file.path(files_path, combined_file_names)]
  
  # dfs = lapply(files, read_csv)
  dfs = lapply(c(ice_file, physics_file, nutrients_file, secchi_file, zoopDensity_file), read_csv)
  
  comb_data = bind_rows(dfs)
  
  # trim to just years with full sampling
  first_years = comb_data %>% 
    group_by(lakeid, metric) %>% 
    summarise(first_year = min(year, na.rm=T)) %>% 
    pivot_wider(names_from = metric, values_from = first_year) %>% 
    t()
  
  colnames(first_years) = first_years[1, ] 
  first_years 
  # Northern lakes 1981 or 1982 except for DOC (1986)
  # Southern lakes variable: 
  ##  ME/MO mostly 1995 but doc = 1996
  ##  FI mostly 1996 but chl = 1997, zoops = 1997
  ##  WI mostly 1996 but chl = 1997, zoops = 2007
  
  # based on first sample dates in physics.R: start in year X for lakes Y
  # 1982: AL, MB, CB, CR, SP, TB, TR
  # 1996: ME, MO, WI, FI
  comb_data_out = comb_data %>%
     filter((lakeid %in% c("AL", "BM", "MB", "CB", "CR", "SP", "TB", "TR") & year >= 1982) |
              (lakeid %in% c("ME", "MO", "WI", "FI") & year >= 1996))
  
  write_csv(comb_data_out, file = path_out)
  return(path_out)

}
