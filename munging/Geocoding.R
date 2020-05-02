# Geocode Businesses_Open_Closed

library(tidyverse)
library(mapview)
library(readxl)
library(sf)

source_path = here::here('data/Businesses Open_Closed in Cambridge.xlsx')
df = read_xlsx(source_path)

# Create a data set for Texas A&M geocoder
to_geocode = df %>% 
  mutate(id=1:nrow(df)) %>% 
  select(id, `Business Address`) %>% 
  filter(!duplicated(`Business Address`), 
         !is.na(`Business Address`),
         !str_detect(`Business Address`, 'location')) %>% 
  mutate(Address = str_replace(`Business Address`, 
                       'Mass ?\\.? Ave\\.?', 'Massachusetts Ave')) %>% 
  mutate(Address = str_replace_all(Address, # fixup for typos
    c('Anve'='Ave', 'Stt'='St', 
      'Masschusetts'='Massachusetts', 'Massaschusetts'='Massachusetts',
      'My\\. Auburn'='Mount Auburn', 'Avve'='Ave', 
      'Blachard'='Blanchard', 'Brooke'='Brook'))) %>% 
  mutate(City='Cambridge', State='MA', Zip='')

write_csv(to_geocode, path=here::here('munging/to_geocode.csv'))
from_geocode <- read_csv(here::here('munging/from_geocode_geocodio.csv')) %>% 
  select("id", "Business Address", "Address",
         "Latitude", "Longitude", "Accuracy Score", "Accuracy Type", 
         Zip="Zip_1", "Source")
from_sf = from_geocode %>% st_as_sf(coords=c('Longitude', 'Latitude'), crs=4326)

# These are bogus. Write a file to edit with corrections
from_geocode %>% 
  filter(`Accuracy Score`<=0.6) %>% 
  select(1:5) %>% 
  write_csv(here::here('munging/geocode_corrections.csv'))

corr = read_csv(here::here('munging/geocode_corrections.csv'))
corr %>% 
  st_as_sf(coords=c('Longitude', 'Latitude'), crs=4326) %>% 
  mapview()

# Now merge this mess
from_geocode[from_geocode$`Accuracy Score`<=0.6, 'Latitude'] = corr$Latitude
from_geocode[from_geocode$`Accuracy Score`<=0.6, 'Longitude'] = corr$Longitude
from_geocode %>% 
  st_as_sf(coords=c('Longitude', 'Latitude'), crs=4326) %>% 
  mapview()

geocoded = df %>% 
  left_join(from_geocode %>% select(-id))

mapview(geocoded %>% 
          filter(!is.na(Latitude)) %>% 
          st_as_sf(coords=c('Longitude', 'Latitude'), crs=4326), 
        zcol='Business Name', legend=FALSE)

geocoded = read_csv(here::here('data/business_open_closed.csv'))

# Find business district, etc for the geocoded addresses
bus_dist = st_read(here::here('COD/ASSESSING_CommercialDistrictsFY2020.gdb.zip'),
                   stringsAsFactors=FALSE)

# st_as_sf fails with NA values so just work with the valid geocodes
to_locate = geocoded %>% 
          filter(!is.na(Latitude)) %>% 
  st_as_sf(coords=c('Longitude', 'Latitude'), crs=4326) %>% 
  st_transform(crs=2249)

by_dist = to_locate %>% 
  st_join(bus_dist %>% select(DIST_NAME, DISTRICT))
by_dist %>% 
  mapview(zcol='DIST_NAME')

# Neighborhood
nbhd = st_read('/Users/kent/Dev/FPRA/Cambridge Open Data/Shapefiles/BOUNDARY_CDDNeighborhoods.shp/BOUNDARY_CDDNeighborhoods.shp', stringsAsFactors=FALSE) %>% 
  select(N_HOOD, NAME)

by_dist = by_dist %>% st_join(nbhd)
by_dist %>% 
  mapview(zcol='NAME')

census = st_read(here::here('COD/DEMOGRAPHICS_Tracts2010.gdb.zip'), 
                 stringsAsFactors=FALSE) %>% 
  select(GEOID10)
by_dist = by_dist %>% st_join(census)
mapview(by_dist, zcol='GEOID10') + mapview(census)

# Join with the full geocoded data
geocoded = geocoded %>% left_join(by_dist %>% st_drop_geometry())
write_csv(geocoded, here::here('data/business_open_closed.csv'))


