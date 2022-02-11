# Do the biomass calculations and then do check ID'd in underlying_data_check.Rmd

library(tidyverse)

# data
data_raw = read_csv("../../../Data/ntl_allzoops_raw.csv")
# biomass calc coefs
coefs_matched = read_csv( "../../../Data/zoop_mass_coefs/mass_coefs_matched_v2.csv")
coefs_matched %>% 
  filter(is.na(c1)) # looks good

data_comb = left_join(data_raw, coefs_matched)
data_comb %>% 
  filter(is.na(larger_group) | is.na(c1)) %>% 
  select(species_name, eqn, larger_group, c1) %>% 
  unique()

# fill in missing lengths
data_comb %>% 
  filter(is.na(avg_length)) %>% 
  nrow() # 66.9K out of 94K -> missing 71%

# see how consistent lengths are
data_comb %>% 
  group_by(lakeid, species_name) %>% 
  summarise(CV = sd(avg_length, na.rm=T) / mean(avg_length, na.rm=T)) %>% 
  arrange(desc(CV)) %>% 
  View() # most of biggest CVs are nauplii, probably okay

# lake specific means
lake_means = data_comb %>% 
  group_by(species_name, lakeid) %>% 
  summarise(mean_length_lake = mean(avg_length, na.rm=T))

# overall means
overall_means = read_csv("../../../Data/zoop_cleaned_data/overall_mean_lengths_filled.csv")


# 
# # ========================================================
# # Old code - used to fill in missing lengths
# # ========================================================
# 
# # overall means
# overall_means = data_comb %>% 
#   group_by(species_name) %>% 
#   summarise(mean_length_overall = mean(avg_length, na.rm=T))
# 
# # fill in specific overall means
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "ALONA KARUA", 
#   ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "ALONA",
#     ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "BRACHIONUS ANGULARIS", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "BRACHIONUS",
#   ][["mean_length_overall"]] 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "BRACHIONUS QUADRIDENTATUS", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "BRACHIONUS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "CONOCHILUS HIPPOCREPIS", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "CONOCHILUS",
#   ][["mean_length_overall"]] 
# 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "DIACYCLOPS", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "DIACYCLOPS THOMASI",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "DIAPHANOSOMA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "DIAPHANOSOMA BIRGEI",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "EUCHLANIS PELLUCIDA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "EUCHLANIS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "FILINIA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "FILINIA TERMINALIS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "HOLOPEDIUM", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "HOLOPEDIUM GIBBERUM",
#   ][["mean_length_overall"]] 
#   
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "KELLICOTTIA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "KELLICOTTIA BOSTONIENSIS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "KERATELLA COCHLEARIS F. TECTA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "KERATELLA COCHLEARIS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "LECANE CLARA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "LECANE",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "LECANE LEONTINA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "LECANE",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "LECANE TUDICOLA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "LECANE",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "LEPADELLA ACUMINATA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "LEPADELLA",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "LEPADELLA TRIPTERA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "LEPADELLA",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "LEPTODORA KINDTII", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "LEPTODORA KINDTI",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "MANFREDIUM", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "MANFREDIUM EUDACTYLOTUM",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "MONOSTYLA STENROOSI", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "MONOSTYLA",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "NOTHOLCA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "NOTHOLCA MICHIGANENSIS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "NOTHOLCA ACUMINATA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "NOTHOLCA MICHIGANENSIS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "NOTHOLCA LABIS", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "NOTHOLCA MICHIGANENSIS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "PLOESOMA TRUNCATUM", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "PLOESOMA",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "POLYARTHRA EURYPTERA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "POLYARTHRA",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "POLYPHEMUS", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "POLYPHEMUS PEDICULUS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "SKISTODIAPTOMUS", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "SKISTODIAPTOMUS OREGONENSIS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "SKISTODIAPTOMUS PALLIDUS", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "SKISTODIAPTOMUS OREGONENSIS",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "TRICHOCERCA CAPUCINA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "TRICHOCERCA",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "TRICHOCERCA LONGISETA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "TRICHOCERCA",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "TRICHOCERCA PUSILLA", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "TRICHOCERCA",
#   ][["mean_length_overall"]] 
# 
# overall_means[
#   !is.na(overall_means$species_name) & overall_means$species_name == "TROPOCYCLOPS", 
# ][["mean_length_overall"]] = 
#   overall_means[
#     !is.na(overall_means$species_name) & overall_means$species_name == "TROPOCYCLOPS PRASINUS MEXICANUS",   ][["mean_length_overall"]] 
# 
# write_csv(overall_means, "../../../Data/zoop_cleaned_data/overall_mean_lengths_filled.csv")
