
install.packages('downloader')
library(downloader)
 

# North Temperate Lakes LTER Yahara Lakes District Bathymetry
url = 'https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.153.10&entityid=1f321e749b6372f03bb5b96390f833cf'
download(url, dest="Data/dataset.zip", mode="wb") 
unzip("Data/dataset.zip", exdir = "./Data/")

# North Temperate Lakes LTER Northern Highland Lake District Bathymetry
url = 'https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.288.2&entityid=88128a1a3a1220023beb3ddf092435da'
download(url, dest="Data/dataset.zip", mode="wb") 
unzip("Data/dataset.zip", exdir = "./Data/")

file.remove('Data/dataset.zip')
