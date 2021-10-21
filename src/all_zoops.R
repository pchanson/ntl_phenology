library(tidyverse)

# read in raw zoops data
zoops = read_csv("./Data/ntl_allzoops_raw.csv")

# read in zoop mass file
masses = read_csv("./Data/Mass_Calc_LTER_uploaded_formulasFormatted.csv")

# string manipulations to pull out constants
inds_pow = masses$eqn == "pow" & !is.na(masses$eqn)

test = masses$mass_formula[1]

gsub("=(.*)\\(.*", x=test, "\\1")
gsub("=.*\\(.*\\^(.*)\\)", x=test, "\\1")

masses[inds_pow, "c1"] = gsub("=(.*)\\*\\(.*", x=pull(masses[inds_pow, "mass_formula"]), "\\1")
masses[inds_pow, "c2"] = gsub("=.*\\(.*\\^(.*)\\)", x=pull(masses[inds_pow, "mass_formula"]), "\\1")
masses[inds_pow, "c2"] = gsub("\\)", x = pull(masses[inds_pow, "c2"]), "")

masses$c2 = as.numeric(masses$c2)
masses$c1 = as.numeric(masses$c1)

# write_csv(masses, "./Data/zoop_mass_formulas_constants_extracted.csv")

# doing the last few ones by hand... read in after
masses_coefs = read_csv("./Data/zoop_mass_formulas_constants_extracted.csv")

# see how well the names match up
head(sort(unique(zoops$species_name)))
masses_coefs$species_name = str_to_upper(masses_coefs$species)
masses_combs = masses_coefs %>% select(species_name, eqn, larger_group, c1, c2, c3)

zoops_table = unique(zoops[, c("species_name")])
hold = full_join(zoops_table, masses_combs) %>% arrange(species_name)

# write_csv(hold, "./Data/mass_coefs_unmatched.csv")
# these got copied over by hand...

coefs_matched = read_csv( "./Data/zoop_mass_coefs/mass_coefs_matched.csv")

# merge mass coefs
zoops_coef = left_join(zoops, coefs_matched)

# fill lengths
lake_specific_mean_lengths = zoops %>%
  group_by(lakeid, species_name) %>% 
  summarise(lake_mean_length = mean(avg_length, na.rm=T))

overall_mean_lengths = zoops %>% 
  group_by(species_name) %>% 
  summarise(overall_mean_length = mean(avg_length, na.rm=T))

zoops_filled = zoops_coef %>% 
  left_join(lake_specific_mean_lengths) %>% 
  left_join(overall_mean_lengths)

zoops_filled = zoops_filled %>% 
  mutate(avg_length_filled = ifelse(!is.na(avg_length), avg_length, lake_mean_length)) %>% 
  mutate(avg_length_filled = ifelse(!is.na(avg_length_filled), avg_length_filled, overall_mean_length))

# calc mass
inds_const = !is.na(zoops_filled$eqn) & zoops_filled$eqn == "const" 
inds_pow = !is.na(zoops_filled$eqn) & zoops_filled$eqn == "pow" 
inds_exp = !is.na(zoops_filled$eqn) & zoops_filled$eqn == "exp" 
inds_comp = !is.na(zoops_filled$eqn) & zoops_filled$eqn == "complex"

# calc biomass
zoops_filled$mass = NA
zoops_filled$mass = as.numeric(zoops_filled$mass)

zoops_filled[inds_const, "mass"] = pull(zoops_filled[inds_const, "c1"])
zoops_filled[inds_pow, "mass"] = pull(zoops_filled[inds_pow, "c1"]) * pull(zoops_filled[inds_pow, "avg_length_filled"]) ^  pull(zoops_filled[inds_pow, "c2"])

zoops_filled[inds_exp, "mass"] = exp(pull(zoops_filled[inds_exp, "c1"]) + pull(zoops_filled[inds_exp, "c2"] * log(pull(zoops_filled[inds_exp, "avg_length_filled"]))))

zoops_filled[inds_comp, "mass"] = pull(zoops_filled[inds_comp, "c1"]) * pull(zoops_filled[inds_comp, "avg_length_filled"]) * (pull(zoops_filled[inds_comp, "c2"]) *  pull(zoops_filled[inds_comp, "avg_length_filled"]) ^  pull(zoops_filled[inds_comp, "c3"]))


ggplot(zoops_filled, aes(x=log10(mass))) +
  geom_histogram()+
  facet_grid(rows = vars(larger_group)) 

zoops_filled$biomass = zoops_filled$mass * zoops_filled$density

ggplot(zoops_filled %>% filter(!is.na(larger_group)), aes(x=log10(biomass)))+
  geom_histogram()+
  facet_grid(cols = vars(larger_group), rows=vars(lakeid), scales="free_y")

zoops_sample_biomasss = zoops_filled %>% 
  group_by(lakeid, sampledate, larger_group) %>% 
  summarise(total_biomass = sum(biomass, na.rm=T))

zoops_sample_total_biomass = zoops_filled %>% 
  group_by(lakeid, sampledate) %>% 
  summarise(total_biomass = sum(biomass, na.rm=T)) %>% 
  mutate(larger_group = "TOTAL")

zoops_grouped_biomass = zoops_sample_biomasss %>% 
  bind_rows(zoops_sample_total_biomass) %>% 
  arrange(lakeid, sampledate, larger_group) %>% 
  rename(group = larger_group, biomass = total_biomass)

# write_csv(zoops_grouped_biomass, "./Data/zoop_cleaned_data/ntl_grouped_zoop_biomass_by_sampledate.csv")

# get date of max biomass by year
just_total = zoops_grouped_biomass %>% 
  filter(group == "TOTAL") %>% 
  mutate(year = lubridate::year(sampledate), doy = lubridate::yday(sampledate))

just_total %>% 
  group_by(lakeid, year) %>% 
  slice_max(biomass)%>% 
  ggplot(aes(x=doy, fill=lakeid)) +
  geom_histogram() +
  facet_grid(rows=vars(lakeid))

sort(unique(just_total$lakeid))

# "AL" "BM" "CB" "CR" "FI" "ME" "MO" "SP" "TB" "TR" "WI"
just_total %>% 
  filter(lakeid == "WI") %>% 
  ggplot(aes(x=doy, y=biomass)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~year, scales="free_y")

max_zoop_biomass = just_total %>% 
  group_by(lakeid, year) %>% 
  slice_max(biomass)

# write_csv(max_zoop_biomass, "./Data/max_totalzoop_biomass.csv")


# compare daphnia to zoop biomass
daphnia = read_csv("./Data/max_daphnia_biomass.csv")
daphnia = daphnia %>% 
  rename(year = year4, biomass = daphnia_biomass) %>% 
  mutate(group = "DAPHNIA")

comb = bind_rows(max_zoop_biomass, daphnia)

ggplot(comb) + 
  stat_density_ridges(aes(x = doy, 
                          y= group, col = group, fill = group), 
                      alpha = 0.5, quantile_lines = T, quantiles = 2) +
  facet_wrap(~ (lakeid)) +
  xlab('DOY') + ylab('Density')+
  theme_minimal() 
