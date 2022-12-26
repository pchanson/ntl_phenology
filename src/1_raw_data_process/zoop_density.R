#### Download southern lake zooplankton data from EDI ####
inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/90/33/5880c7ba184589e239aec9c55f9d313b"
infile1 <- tempfile()
download.file(inUrl1, infile1, method = "curl")
dt1 <- read_csv(infile1)

#### Download nothern lake zooplankton data from EDI ####
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/36/c4b652eea76cd431ac5fd3562b1837ee" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
dt2 <-read_csv(infile1) |> rename(sampledate = sample_date)

dt = dt1 |> select(-towdepth) |> bind_rows(dt2)

# Zooplankton ID codes
codes = dt |> 
  group_by(species_code) |> 
  summarise(first(species_name))


# by zoop group
zoops = dt |> 
  mutate(code = floor(species_code/10000)) |>
  group_by(lakeid, sampledate, code) |> 
  summarize(density = sum(density, na.rm = T)) |> 
  mutate(month = month(sampledate), year = year(sampledate)) |> 
  mutate(zoopGroup = case_when(code == 1 ~ 'copepod nauplii',
                               code == 2 ~ 'copepod',
                               code == 3 ~ 'calanoid',
                               code == 4 ~ 'harpacticoid',
                               code == 5 ~ 'cladocera',
                               code == 6 ~ 'rotifer',
                               code == 7 ~ 'unknown',
                               code == 8 ~ 'unknown',
                               code == 9 ~ 'unknown'))

# all zoops
zoopDensity = dt |> 
  group_by(lakeid, sampledate) |> 
  summarize(density = sum(density, na.rm = T)) |> 
  mutate(month = month(sampledate), year = year(sampledate)) |> 
  filter(month >= 4) |> 
  group_by(lakeid, year) |> 
  slice_max(density) |> 
  mutate(metric = 'zoopDensity', daynum = yday(sampledate))

ggplot(zoopDensity) + 
  geom_point(aes(x = month, y = density)) +
  facet_wrap(~lakeid, scales = 'free_y')

write_csv(zoopDensity |>  select(lakeid, metric, sampledate, year, daynum), 
          "Data/final_metric_files/ntl_zoop_density.csv")
