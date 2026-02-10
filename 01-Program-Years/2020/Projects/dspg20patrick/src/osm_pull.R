library(osmdata)
library(sf)
library(sp)
library(leaflet)
library(tidyverse)
library(foreign)

############################# OSM Data #############################

# search for your place of interest
# coordinates of patrick county 36.6886° N, 80.3213° W
bb <- getbb('patrick county, virginia')

# from that place get a key and the specific value
q <- opq(bbox = bb) %>%
  add_osm_feature(key = 'amenity', value = c('pharmacy', 'hospital', 'clinic', 'doctors', 'dentist',
                                             'nursing_home', 'social_facility', 'ambulance_station'))
osmdata_xml(q, '~/git/dspg2020patrick/data/working/patrick.osm')
  
# r <- opq(bbox = bb) %>%
#   add_osm_feature(key = 'highway', value = c('primary', 'secondary', 'tertiary'))
# osmdata_xml(r, 'patrick_02.osm')

medical <- osmdata_sf(q, '~/git/dspg2020patrick/data/working/patrick.osm')$osm_points


r <- opq(bbox = bb) %>%
  add_osm_feature(key = 'shop', value = c('convenience', 'supermarket', 'greengrocer', 'farm',
                                          'health_food'))
osmdata_xml(r, '~/git/dspg2020patrick/data/working/patrick_food.osm')
food <- osmdata_sf(r, '~/git/dspg2020patrick/data/working/patrick_food.osm')$osm_points


# leaflet ---------------------------------------------

osm_medical_map <- leaflet(data=medical) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers()
osm_medical_map # Print the map

osm_food_map <- leaflet(data=food) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers()
osm_food_map # Print the map



