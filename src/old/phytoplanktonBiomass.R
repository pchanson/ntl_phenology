# Package ID: knb-lter-ntl.88.28 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Phytoplankton - Madison Lakes Area 1995 - current.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/88/31/f2de15b2fff6ae962a04c150c0a1c510" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="auto")
phytos <- read_csv(infile1)

# get ice on/off dates
ice0 = read_csv("Data/derived/ntl_icedatescombo.csv") |> 
  mutate(year4 = year(lastice)) |> 
  filter(year4 >= 1980) |> 
  select(-year)

puse = phytos |> 
  left_join(ice0) |> 
  filter(sampledate > lastice) |> 
  filter(lakeid %in% c('ME','MO')) |> 
  filter((lakeid == 'ME' & depth_range == '0-8m') | (lakeid == 'MO' & depth_range == '0-2m'))

####### all phytos ########
a = puse |> 
  group_by(lakeid, year4, sampledate, depth_range) |> 
  summarise(biovolume_conc = sum(biovolume_conc)) |> 
  group_by(lakeid, year4) |> 
  slice_max(biovolume_conc, with_ties = FALSE, n = 1) |> 
  mutate(daynum = yday(sampledate))

ggplot(a) +
  geom_point(aes(x = daynum, y = biovolume_conc, color = year4)) +
  facet_wrap(~lakeid)

####### Cyanophyta ########
b = puse |> 
  filter(division == 'Cyanophyta') |> 
  group_by(lakeid, year4, sampledate, depth_range) |> 
  summarise(biovolume_conc = sum(biovolume_conc)) |> 
  group_by(lakeid, year4) |> 
  slice_max(biovolume_conc, with_ties = FALSE, n = 1) |> 
  mutate(daynum = yday(sampledate))

ggplot(b) +
  geom_point(aes(x = daynum, y = biovolume_conc, color = year4)) +
  facet_wrap(~lakeid)

####### Bacillariophyta ########
d = puse |> 
  filter(division == 'Bacillariophyta') |> 
  group_by(lakeid, year4, sampledate, depth_range) |> 
  summarise(biovolume_conc = sum(biovolume_conc)) |> 
  group_by(lakeid, year4) |> 
  slice_max(biovolume_conc, with_ties = FALSE, n = 1) |> 
  mutate(daynum = yday(sampledate))

ggplot(d) +
  geom_point(aes(x = daynum, y = biovolume_conc, color = year4)) +
  facet_wrap(~lakeid)
