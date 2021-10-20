
# install.packages('downloader')
library(downloader)
library(tidyverse)
library(sf)


# # North Temperate Lakes LTER Yahara Lakes District Bathymetry
# url = 'https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.153.10&entityid=1f321e749b6372f03bb5b96390f833cf'
# download(url, dest="Data/dataset.zip", mode="wb") 
# unzip("Data/dataset.zip", exdir = "./Data/")
# 
# # North Temperate Lakes LTER Northern Highland Lake District Bathymetry
# url = 'https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.288.2&entityid=88128a1a3a1220023beb3ddf092435da'
# download(url, dest="Data/dataset.zip", mode="wb") 
# unzip("Data/dataset.zip", exdir = "./Data/")

Remove zip file
file.remove('Data/dataset.zip')

# Read bathymetry shapefiles
nhd = st_read('GIS/nhld_bathymetry.shp')

nhd.sum = nhd %>% mutate(area = st_area(.)) %>% 
  group_by(LakeID, Depth_m, Depth_ft) %>% 
  summarise(area = sum(area)) %>% 
  st_drop_geometry() %>% 
  rename(lakeid = LakeID) %>% 
  select(lakeid, Depth_m, Depth_ft, area)

# Read bathymetry shapefiles
mendota = st_read('GIS/mendota-contours-all.shp') %>% 
  st_set_crs(st_crs(nhd)) %>% 
  mutate(lakeid = "ME") %>% 
  mutate(area = st_area(.)) %>% 
  group_by(lakeid, Depth_m, Depth_ft) %>% 
  summarise(area = sum(area)) %>% 
  st_drop_geometry() %>% 
  select(lakeid, Depth_m, Depth_ft, area)

monona = st_read('GIS/monona-contours-all.shp') %>% 
  st_set_crs(st_crs(nhd)) %>% 
  mutate(lakeid = "MO") %>% 
  mutate(area = st_area(.)) %>% 
  group_by(lakeid, Depth_m, Depth_ft) %>% 
  summarise(area = sum(area)) %>% 
  st_drop_geometry() %>% 
  select(lakeid, Depth_m, Depth_ft, area)

wingra = st_read('GIS/wingra-contours-all.shp') %>% 
  st_set_crs(st_crs(nhd)) %>% 
  mutate(lakeid = "WI") %>% 
  mutate(area = st_area(.)) %>% 
  group_by(lakeid, Depth_m, Depth_ft) %>% 
  summarise(area = sum(area)) %>% 
  st_drop_geometry() %>% 
  select(lakeid, Depth_m, Depth_ft, area)

hypsometry.df = bind_rows(nhd.sum, mendota) %>% 
  bind_rows(monona) %>% 
  bind_rows(wingra)

write_csv(hypsometry.df, 'Data/NTLhypsometry.csv')
