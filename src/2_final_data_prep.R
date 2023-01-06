# packages needed for these targets
tar_option_set(packages = c("lubridate",
                            "tidyverse"))


source('src/2_final_data_prep/00_combine_final_files.R')
source('src/2_final_data_prep/01_missing_dates_fill.R')

p2_targets_list <- list(
  tar_target(
    name = combine_final_files_csv,
    combine_final_files(
      ice_file = ice_csv,
      physics_file = physics_csv,
      nutrients_file = nutrients_csv,
      secchi_file = secchi_csv,
      zoopDensity_file = zoopDensity_csv,
      path_out = "Data/final_metric_files/final_combined_dates.csv"),
    format = "file"
  ),
  tar_target(
    name = missing_dates_fill_csv,
    missing_dates_fill(
      path_in = combine_final_files_csv,
      path_out = "Data/analysis_ready/final_combined_dates_filled_v2.csv"),
    format = "file"
  )
)