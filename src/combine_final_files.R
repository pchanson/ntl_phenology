# read in and combine final phenology dates

library(tidyverse)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

files_path = "../Data/final_metric_data"
combined_file_names = "final_combined_dates.csv"

files0 = list.files(files_path, full.names = T)
files = files0[files0 != file.path(files_path, combined_file_names)]

dfs = lapply(files, read_csv)
comb_data = bind_rows(dfs)

write_csv(comb_data, file.path(files_path, combined_file_names))