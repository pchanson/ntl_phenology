# packages needed for these targets
tar_option_set(packages = c("lubridate",
                            "tidyverse",
                            "scales",
                            "MetBrewer",
                            "ggridges",
                            "patchwork",
                            "ggcorrplot",
                            "corrr"))

source("src/3_figures/Figure1_ggridges.R")
source("src/3_figures/Figure1_ggridges_v2.R")
source("src/3_figures/Figure2_betweenLake.R")
source("src/3_figures/Figure3_WithinLake.R")

p3_targets_list <- list(
  tar_target(
    name = figure1_png,
    figure1(path_in = 'Data/analysis_ready/final_combined_dates_filled_v2.csv',
      path_out = "Figures_manuscript/Figure1.png")
  ),
  tar_target(
    name = figure1_v2_png,
    figure1_v2(path_in = 'Data/analysis_ready/final_combined_dates_filled_v2.csv',
            path_out = "Figures_manuscript/Figure1_v2.pdf")
  ),
  tar_target(
    name = figure2_png,
    figure2(path_in = 'Data/analysis_ready/final_combined_dates_filled_v2.csv',
            path_out = "Figures_manuscript/Figure2.png")
  ),
  tar_target(
    name = figure3_png,
    figure3(path_in = 'Data/analysis_ready/final_combined_dates_filled_v2.csv',
            path_out = "Figures_manuscript/Figure3.png")
  )

)