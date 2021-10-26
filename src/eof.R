library(tidyverse)
# devtools::install_github("marchtaylor/sinkr")
library(sinkr)
source("timing_matrix_construction.R")
timings = read_csv("../Data/all_event_timing_differences.csv")
years = matrix_single_lake(timings, "ME")[,,18:41]

pull_upper <- function(x){
  x[lower.tri(x)]
}

years_2d = apply(years, 3, pull_upper)

E = eof(years_2d, nu=5)
plot(as.numeric(dimnames(years_2d)[[2]]), E$u[,2])

lakes = unique(timings$id)
for(i in 1:length(lakes)){
  years = matrix_single_lake(timings, "ME")[,,as.character(1996:2019)]
  years_2d = as.data.frame(apply(years, 3, pull_upper))
  years_2d$Lake = lakes[i]
  
  if(i == 1){
    hold = years_2d
  }else{
    hold = bind_rows(hold, years_2d)
  }
}

E = eof(hold[, as.character(1996:2019)], nu=5)
