# Geocode Businesses_Open_Closed

library(tidyverse)
library(readxl)

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

write_csv(geocoded, here::here('data/business_open_closed.csv'))
