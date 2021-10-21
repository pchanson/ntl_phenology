library(tidyverse)

# read in raw zoops data
zoops = read_csv("./Data/ntl_allzoops_raw.csv")

# read in zoop mass file
masses = read_csv("./Data/Mass_Calc_LTER_uploaded_formulasFormatted.csv")
