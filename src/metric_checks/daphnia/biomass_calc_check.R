# Do the biomass calculations and then do check ID'd in underlying_data_check.Rmd

library(tidyverse)
library(ggridges)

# data
data_raw = read_csv("../../../Data/ntl_allzoops_raw_v2.csv")
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

# do the filling
data_comb = left_join(data_comb, lake_means)
data_comb = left_join(data_comb, overall_means)
data_comb = data_comb %>% 
  mutate(avg_length = ifelse(is.na(avg_length), mean_length_lake, avg_length))

data_comb %>% 
  filter(is.na(avg_length)) %>% 
  select(lakeid, species_name) %>% 
  unique() %>% View()
data_comb = data_comb %>% 
  mutate(avg_length = ifelse(is.na(avg_length), mean_length_overall, avg_length))
data_comb %>% 
  filter(is.na(avg_length)) %>% 
  select(lakeid, species_name) %>% 
  unique() %>% View() # looks good

# do the biomass calc
table(data_comb$eqn, useNA = 'ifany')
data_comb %>% filter(is.na(eqn)) %>% pull(species_name) %>% table()
inds_const = !is.na(data_comb$eqn) & data_comb$eqn == "const" 
inds_pow = !is.na(data_comb$eqn) & data_comb$eqn == "pow" 
inds_exp = !is.na(data_comb$eqn) & data_comb$eqn == "exp" 
inds_comp = !is.na(data_comb$eqn) & data_comb$eqn == "complex"

# apply the equations
data_comb$mass = NA
data_comb$mass = as.numeric(data_comb$mass)

data_comb[inds_const, "mass"] = pull(data_comb[inds_const, "c1"])
data_comb[inds_pow, "mass"] = pull(data_comb[inds_pow, "c1"]) * pull(data_comb[inds_pow, "avg_length"]) ^  pull(data_comb[inds_pow, "c2"])

data_comb[inds_exp, "mass"] = exp(pull(data_comb[inds_exp, "c1"]) + pull(data_comb[inds_exp, "c2"]) * log(pull(data_comb[inds_exp, "avg_length"])))

data_comb[inds_comp, "mass"] = pull(data_comb[inds_comp, "c1"]) * pull(data_comb[inds_comp, "avg_length"]) * ((pull(data_comb[inds_comp, "c2"]) *  pull(data_comb[inds_comp, "avg_length"])) ^  2)

# see if these worked
data_comb %>% 
  filter(!is.na(c1) & is.na(mass)) %>% 
  pull(species_name) %>% 
  table(useNA = 'ifany') # looks good; just the ones expecting

# get density in same units
data_comb$density_nPerL = as.numeric(NA)
# make sure all S lakes have towdepth
data_comb %>% 
  filter(is.na(towdepth)) %>% 
  pull(lakeid) %>% 
  table() # good; only the northern lakes have missing towdepths
N_lakes = c("AL", "BM", "CB", "CR", "SP", "TB", "TR")
data_comb = data_comb %>% 
  mutate(density_nPerL = ifelse(lakeid %in% N_lakes, density, density / (1000 * towdepth)))

# calc biomass
data_comb$biomass_perL = data_comb$mass * data_comb$density_nPerL

# calc grouped totals
zoop_grouped_biomass = data_comb %>% 
  group_by(lakeid, year4, sampledate, larger_group) %>% 
  summarise(total_biomass = sum(biomass_perL, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(larger_group))

# add zeros
all_combos = zoop_grouped_biomass %>% 
  # group_by(lakeid, sampledate) %>% 
  expand(nesting(lakeid, sampledate, year4), larger_group)

zoop_grouped_biomass_filled = full_join(zoop_grouped_biomass, all_combos)
zoop_grouped_biomass_filled = zoop_grouped_biomass_filled %>% 
  mutate(total_biomass = ifelse(is.na(total_biomass), 0, total_biomass))

zoop_grouped_biomass_filled %>% 
  filter(lakeid == "AL") %>% 
  ggplot(aes(x=sampledate, y=total_biomass)) +
  geom_area(aes(color = larger_group, fill=larger_group)) +
  facet_wrap(~year4, scales="free_x") +
  theme_bw()

zoop_total_biomass = data_comb %>% 
  group_by(lakeid, sampledate) %>% 
  summarise(total_biomass = sum(biomass_perL, na.rm=T)) %>% 
  mutate(larger_group = "TOTAL") %>% 
  ungroup()

# pull out / calculate just daphnia
daphnia_spp = c("DAPHNIA", "DAPHNIA AMBIGUA", "DAPHNIA DENTIFERA", "DAPHNIA DUBIA", "DAPHNIA LONGIREMIS", "DAPHNIA MENDOTAE", "DAPHNIA PARVULA", "DAPHNIA PULICARIA", "DAPHNIA RETROCURVA")

daphnia_grouped_biomass = data_comb %>% 
  filter(species_name %in% daphnia_spp) %>% 
  group_by(lakeid, year4, sampledate) %>% 
  summarise(total_biomass = sum(biomass_perL, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(larger_group = "DAPHNIA")

daphnia_grouped_biomass %>% 
  filter(lakeid == "AL") %>% 
  ggplot(aes(x=sampledate, y=total_biomass)) +
  geom_area(aes(color = larger_group, fill=larger_group)) +
  facet_wrap(~year4, scales="free_x") +
  theme_bw()

# # save files
# write_csv(data_comb, "../../../Data/ntl_all_zoop_info_species_level.csv")
# write_csv(zoop_grouped_biomass_filled, "../../../Data/ntl_cladocera_copepoda_rotifera_biomass.csv")
# write_csv(daphnia_grouped_biomass, "../../../Data/ntl_daphnia_biomass.csv")

# calc DOY of max biomass
zoop_max = zoop_total_biomass %>% 
  mutate(year4 = lubridate::year(sampledate)) %>% 
  group_by(lakeid, year4) %>% 
  slice_max(total_biomass)

daphnia_max = daphnia_grouped_biomass %>% 
  group_by(lakeid, year4) %>% 
  slice_max(total_biomass)

both_max = bind_rows(zoop_max, daphnia_max) %>% 
  rename(group=larger_group)

# WRITE results
both_max_out = both_max %>% 
  rename(metric = group, year = year4) %>% 
  select(-total_biomass) %>% 
  mutate(metric = ifelse(metric == "TOTAL", "total_zoop_biomass", metric)) %>% 
  mutate(metric = ifelse(metric == "DAPHNIA", "daphnia_biomass", metric)) %>% 
  mutate(daynum = lubridate::yday(sampledate)) %>% 
  select(lakeid, metric, sampledate, year, daynum)

# write_csv(both_max_out, "../../../Data/final_metric_data/zooplankton_max_biomass.csv")

# plot each lake
lakes = c("AL", "TR", "BM", "CR", "SP", "TB", "CB", "ME", "MO", "WI", "FI")

pdf("../../../Figures/zoop_biomass_larger_groups.pdf", width=11, height=8.5)
for(i in 1:length(lakes)){
  p = zoop_grouped_biomass_filled %>% 
    filter(lakeid == lakes[i]) %>% 
    ggplot(aes(x=sampledate, y=total_biomass)) +
    geom_area(aes(fill=larger_group)) +
    facet_wrap(~year4, scales="free_x") +
    theme_bw() +
    ggtitle(paste(lakes[i], "Zoop Biomass", sep=" - "))
  maxes = both_max %>% 
    filter(lakeid == lakes[i]) %>% 
    mutate(sampledate=ifelse(group == "DAPHNIA", sampledate + 2, sampledate - 2))
  p = p +
    geom_vline(data=maxes, mapping=aes(color=group, xintercept=sampledate), size=1.05) +
    labs(color="Max. Biomass")
  print(p)
}
dev.off()

# TODONE: plot of distribution of biomass of different groups by lake
zoop_grouped_biomass_filled %>% 
  bind_rows(zoop_total_biomass) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes, ordered = T)) %>% 
  mutate(total_biomass = ifelse(larger_group != "TOTAL" & total_biomass > 800, 800, total_biomass)) %>% 
  mutate(total_biomass = ifelse(larger_group == "TOTAL" & total_biomass > 1000, 1000, total_biomass)) %>% 
  ggplot(aes(x=total_biomass, y=lakeid, fill=lakeid)) +
  geom_density_ridges( stat = "binline", bins = 40, scale = 0.95, draw_baseline = FALSE) +
  facet_wrap(~larger_group, scales="free_x") +
  theme_bw()

# TODO: checks from underlying_data_check.Rmd
# TODO: maybe exclude first and last sample from max calculation?


# # ========================================================
# # Old code - used to check and re-create the raw data file w/out duplicating N lakes data
# # ========================================================
# data_comb %>% 
#   filter(is.na(density_nPerL)) %>% 
#   View() #TODONE: issues: BM 2007, 2017; CR 2008; MO 2006; SP 2006, 2012; TB 2008; TR 2006, 2007, 2008
# # all have dates where many species have NA density, but either individuals measured or avg_lengths taht are entered...
# # Question: are there "actual" measurements with density values on those same dates?
# # Answer: -> some have all NA's, some just missing a few spp that had measurements but no densities; summing over a given data should solve
# # TODO / NOTE: summing all NAs w/ na.rm=T will give a 0; make sure to replace with NA
# 
# check_data = data_comb %>% 
#   filter(is.na(density_nPerL)) %>% 
#   select(lakeid, sampledate) %>% 
#   unique() %>% 
#   left_join(data_comb) %>% 
#   arrange(lakeid, sampledate, species_name)
# 
# View(check_data) # some lake-sampledates have all NAs for density; others just missing one spp; 
# # TODONE: all have duplicates for lengths; did I make them or are they in original data? -> my issue in making raw data file in daphnia.R; fixed on "Old Code" below
# check_data_raw = data_comb %>% 
#   filter(is.na(density_nPerL)) %>% 
#   select(lakeid, sampledate) %>% 
#   unique() %>%
#   left_join(data_raw)
# nrow(check_data)
# nrow(check_data_raw) # same; issue is with original data
# nrow(unique(check_data)) # roughly half of data are duplicates; TODO is that true of all data?
# nrow(unique(data_comb)) # 51588
# nrow(data_comb) # NOT GOOD: lots of data are duplicates?
# nrow(unique(data_raw)) # TODONE: issue is present in "../../../Data/ntl_allzoops_raw.csv"; did I make the duplicates when I created it or are they in underlying data? -> I made the duplicates, probably by bind_rows()-ing twice; fixed below in "Old Code"


# check original files
# library(NTLlakeloads)
# zoops = loadLTERzooplankton() # doesn't work; need to use libcurl
# inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/90/31/5880c7ba184589e239aec9c55f9d313b"
# infile1 <- tempfile()
# download.file(inUrl1, infile1, method = "libcurl")
# zoops_south <- read_csv(infile1, skip = 1, quote = "\"", guess_max = 1e+05, 
#                         col_names = c("lakeid", "year4", "sample_date", "station", 
#                                       "towdepth", "species_code", "species_name", "density", 
#                                       "individuals_measured", "avg_length")) %>% rename(sampledate = sample_date)
# 
# inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/36/c4b652eea76cd431ac5fd3562b1837ee" 
# infile2 <- tempfile() 
# download.file(inUrl2,infile2,method="libcurl")
# 
# zoops_north <- read_csv(infile2, quote ='"',guess_max = 100000) %>%
#   rename(sampledate = sample_date)
# 
# nrow(zoops_south)
# nrow(unique(zoops_south)) # south lakes are okay
# nrow(zoops_north)
# nrow(unique(zoops_north)) # also okay???
# 
# zoops_all = bind_rows(zoops_south, zoops_north) %>% 
#   mutate(lakeid = ifelse(lakeid == "Tr", "TR", lakeid))
# nrow(zoops_all) 
# nrow(unique(zoops_all)) # also okay...
# 
# nrow(zoops_all) + nrow(zoops_north)
# nrow(data_comb) # must have rbinded the north lakes data 2x in making original file
# write_csv(zoops_all, "../../../Data/ntl_allzoops_raw_v2.csv")

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
