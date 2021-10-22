library(tidyverse)

timings = read_csv("../Data/all_event_timing_differences.csv")

matrix_single_year <- function(timings.df = timings, single_year=2015){
  
  cur_year = timings.df %>% 
    filter(year == single_year) %>% 
    select(id, Var1, Var2, doy_diff) %>% 
    arrange(id, Var1, Var2)
  
  hold = array(unlist(cur_year$doy_diff), dim = c(11,11,11))
  dimnames(hold) = list(sort(unique(cur_year$Var1)), sort(unique(cur_year$Var2)), sort(unique(cur_year$id)))
  
  return(hold)
}

test = matrix_single_year()
test[,,"BM"]
