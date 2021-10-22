library(NTLlakeloads)
library(tidyverse)

zoops = loadLTERzooplankton()

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/36/c4b652eea76cd431ac5fd3562b1837ee" 
infile2 <- tempfile() 
download.file(inUrl2,infile2,method="curl")

zoops_north <- read_csv(infile2, quote ='"',guess_max = 100000) %>%
  rename(sampledate = sample_date)

zoops_all = bind_rows(zoops, zoops_north) %>% 
  mutate(lakeid = ifelse(lakeid == "Tr", "TR", lakeid))

write_csv(zoops_all, "./Data/ntl_allzoops_raw.csv")

daph_spp = c("DAPHNIA", "DAPHNIA AMBIGUA", "DAPHNIA DENTIFERA", "DAPHNIA DUBIA", "DAPHNIA LONGIREMIS", "DAPHNIA MENDOTAE", "DAPHNIA PARVULA", "DAPHNIA PULICARIA", "DAPHNIA RETROCURVA")

zoops_all %>% 
  filter(species_name %in% daph_spp) %>% 
  ggplot(aes(x=avg_length, fill = species_name)) +
  geom_histogram() +
  facet_grid(cols=vars(species_name), rows=vars(lakeid)) 
# D. Mendotae very different by lake e.g. ME/MO vs. TR/SP; calc averages separately

zoops_all %>% 
  group_by(lakeid, year4) %>% 
  filter(species_name %in% daph_spp) %>% 
  summarise(nMeasurements = sum(!is.na(avg_length)), nNA = sum(is.na(avg_length))) %>% 
  pivot_longer(cols = c("nMeasurements", "nNA")) %>% 
  ggplot(aes(x=year4, y=value, color=name)) +
  geom_point() +
  facet_wrap(~lakeid)

## calculate biomass using cascade equation:
# weight (ug) <- e^(log(m) + b*log(length_mm))
B = 2.7200
M = 1.9445

daphnia_all = zoops_all %>% 
  filter(species_name %in% daph_spp) %>% 
  mutate(avg_biomass = exp(log(M) + B*log(avg_length))) %>% 
  mutate(total_biomass = density*avg_biomass)

# see which spp are present for filling lengths
daphnia_all %>% 
  filter(is.na(total_biomass)) %>% 
  select(species_name) %>% 
  table()

# calcualte spp-specific mean lengths
mean_lengths = daphnia_all %>% 
  group_by(lakeid, species_name) %>% 
  summarise(mean_length = mean(avg_length, na.rm=T))

daphnia_all = daphnia_all %>% 
  left_join(mean_lengths) %>% 
  mutate(avg_length_filled = ifelse(!is.na(avg_length), avg_length, mean_length))

sum(is.na(daphnia_all$avg_length_filled)) # still missing lengths; fill with overall mean length

overall_mean = mean(daphnia_all$avg_length, na.rm=T)

daphnia_all = daphnia_all %>%
  mutate(avg_length_filled = ifelse(!is.na(avg_length_filled), avg_length_filled, overall_mean)) %>% 
  mutate(avg_biomass_filled = exp(log(M) + B*log(avg_length_filled))) %>% 
  mutate(total_biomass_filled = density*avg_biomass_filled)

# calc date-averaged biomass
daphnia_biomass = daphnia_all %>% 
  group_by(lakeid, sampledate) %>% 
  summarise(daphnia_biomass = sum(total_biomass_filled, na.rm=T)) %>% 
  mutate(year4 = lubridate::year(sampledate), doy = lubridate::yday(sampledate))

# look at a plot
daphnia_biomass %>% 
  filter(lakeid == "AL") %>% 
  ggplot(aes(x=doy, y=daphnia_biomass, color=lakeid)) + 
  geom_line() +
  facet_wrap(~year4)

max_biomass = daphnia_biomass %>% 
  group_by(lakeid, year4) %>% 
  slice_max(daphnia_biomass, with_ties = FALSE) 
  
  
max_biomass %>% 
  ggplot(aes(x=doy, fill=lakeid)) +
  geom_histogram() +
  facet_grid(rows=vars(lakeid))
  
write_csv(max_biomass, "./Data/max_daphnia_biomass.csv")
