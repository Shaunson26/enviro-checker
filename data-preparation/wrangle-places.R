library(sf)
library(rmapshaper)
library(dplyr)

mesh <- sf::read_sf('../../../../Downloads/climatevegcovermmbsua2016/Shapefile/VegCover_MMB_SUA_2016.shp')

st_bbox(mesh)

mesh <-
  mesh %>% 
  st_transform(crs = 4326)

places <- read.csv('data/places.csv')

places_sf <-
  places %>% 
  mutate(across(c("LONGITUDE", "LATITUDE"), .fns = function(x) x/10e3)) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

sf_use_s2(FALSE)

geo_matches <- st_intersects(places_sf, mesh)

geo_info <-
  mesh %>% 
  slice(as.numeric(geo_matches)) %>% 
  st_drop_geometry() %>% 
  select(MB_CODE16,  LGA, Region, District, LandType, MB_Reclass)

bind_cols(
  places,
  geo_info
) %>% 
  dplyr::rename(MB_2016_CODE = MB_CODE16) %>% 
  readr::write_csv('data/places.csv')

