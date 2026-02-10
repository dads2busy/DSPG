##################### Isochrones #####################

# devtools::install_github("tlorusso/traveltime")
# devtools::install_github("rCarto/osrm")
# install.packages("rmapzen")
# install.packages("rgdal")
library(traveltime)
library(tidyverse)
library(sf)
library(osmdata)
library(leaflet)
library(sp)
library(purrr)
library(mapview)
library(osrm)
library(rmapzen)
library(rgdal)
# webshot::install_phantomjs()

fips <- c(
  # patrick county zipcode only
  "51141"
  # , "37169", "37171", "51035", "51063", "51067", "51089"
)

#Defining a function that transforms shp file to sf file
#takes in folder, file as argument to give correct path of file
shp_to_sf <- function(folder, file){
  sf::read_sf((paste("./data/original/dhs-", folder, "/", file , ".shp",
                     sep = ""))) %>%
    # only keep data in the places we want
    subset(FIPS %in% fips) %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
}

emsstations <- shp_to_sf("emsstations","emsstations")

# emsstations_plot <- leaflet(data = emsstations) %>% # create leaflet object
#   addProviderTiles(provider = "CartoDB.Positron") %>% # add basemap
#   addMarkers()
# #call plot
# emsstations_plot

# Write out for web
ems <- emsstations %>% st_transform(4269)
ems <- ems %>% select(OBJECTID, ADDRESS, CITY, STATE, ZIP, NAICSDESCR, LONGITUDE, LATITUDE, DIRECTIONS, NAME, geometry)
write_rds(ems, "./data/web/ems.Rds")

#resedential information
residential <- read_sf("./data/working/corelogic/residential.csv")
residential_sf <- st_as_sf(residential, coords = c("parcel_level_longitude", "parcel_level_latitude"))
st_crs(residential_sf) <- "+proj=longlat +datum=WGS84"


# traveltime ----------------------------------------------------------------------
readRenviron("~/.Renviron")
traveltime_api <- Sys.getenv("TRAVELAPI")
traveltime_id <- Sys.getenv("TRAVELID")

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

for(i in 1:nrow(emsstations)){
  i=9
  ems_iso8 <- traveltime_map(appId= traveltime_id,
                             apiKey = traveltime_api,
                             location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                             traveltime=480,
                             type="driving",
                             departure="2020-08-07T08:00:00+01:00")
  saveRDS(ems_iso8, file = paste0('ems_iso_8_',i,'.RDS'))
  ems_iso10 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                              traveltime=600,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(ems_iso10, file = paste0('ems_iso_10_',i,'.RDS'))
  ems_iso12 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(emsstations$LATITUDE[i],emsstations$LONGITUDE[i]),
                              traveltime=720,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(ems_iso12, file = paste0('ems_iso_12_',i,'.RDS'))
  residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
  m1 = mapview(ems_iso8, layer.name = "8 minute isochrone", col.regions = colors[1])
  m2 = mapview(ems_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
  m3 = mapview(ems_iso12, layer.name = "12 minute isochrone", col.regions = colors[3])
  # add the second layer on top
  m1 = m1 + m2 + m3 + residential
  mapshot(m1, file = paste0("~/git/dspg2020patrick/output/isochrone_maps/emsmap_",i, ".png", sep = ""))
}
# residential coverage -------------------------------------------------------
ems_iso8_1 <- readRDS("~/git/dspg20patrick/data/working/isochrones/ems/ems_iso_8_1.RDS")
ems_iso10_1 <- readRDS("~/git/dspg20patrick/data/working/isochrones/ems/ems_iso_10_1.RDS")
ems_iso12_1 <- readRDS("~/git/dspg20patrick/data/working/isochrones/ems/ems_iso_12_1.RDS")

pp_8 <- st_intersection(residential_sf, ems_iso8_1)
pp_10 <- st_intersection(residential_sf, ems_iso10_1)
pp_12 <- st_intersection(residential_sf, ems_iso12_1)

coverage_8_1 <- nrow(pp_8)/nrow(residential_sf)
coverage_10_1 <- nrow(pp_10)/nrow(residential_sf)
coverage_12_1 <- nrow(pp_12)/nrow(residential_sf)

# osrm ------------------------------------------------------------------------

# for(i in 1:nrow(emsstations)){
# iso <- osrmIsochrone(loc = c(emsstations$LONGITUDE[i], emsstations$LATITUDE[i]), breaks = seq(from = 10,to = 20, by = 5))
#
# iso@data$drive_times <- factor(paste(iso@data$min, "to", iso@data$max, "min"))
# # color palette for each area
# factpal <- colorFactor(rev(heat.colors(3)), iso@data$drive_times)
#
# # draw map
# m2 <- leaflet() %>%
#   setView(emsstations$LONGITUDE[i], emsstations$LATITUDE[i], zoom = 11) %>%
#   addProviderTiles("CartoDB.Positron", group="Greyscale") %>%
#   addMarkers(lng = emsstations$LONGITUDE[i], emsstations$LATITUDE[i], popup = "EMS Station Isochrone") %>%
#   addPolygons(fill=TRUE, stroke=TRUE, color = "black",
#               fillColor = ~factpal(iso@data$drive_times),
#               weight=0.5, fillOpacity=0.2,
#               data = iso, popup = iso@data$drive_times,
#               group = "Drive Time") %>%
#   # Legend
#   addLegend("bottomright", pal = factpal, values = iso@data$drive_time,   title = "Drive Time")
# mapshot(m2, file = paste0("~/git/dspg2020patrick/output/isochrone_maps/emsmap_osrm_",i, ".png", sep = ""))
# }

# groceries ------------------------------------------------------------------------
#read in csv file
groceries_full <- read_sf("./data/working/geocode/patrick_groceries.csv")

#Adding lat/long data for missing values, using google maps info

groceries_full$latitude[13] <-  36.7167901
groceries_full$longitude[13] <- -80.4883632
groceries_full$latitude[23] <- 36.6375629
groceries_full$longitude[23] <- -80.2710482

#Changing the long and lat data to numeric and filtering out convenience store
groceries <- groceries_full %>%
  mutate(lat = unlist(lapply(groceries_full$latitude,as.numeric)),
         long = unlist(lapply(groceries_full$longitude, as.numeric))) %>%
  filter( type != "convenience store")


#Changing the csv to sf and the CRS
groceries_sf <- st_as_sf(groceries, coords = c("longitude", "latitude"))

st_crs(groceries_sf) <- "+proj=longlat +datum=WGS84"
nrow(groceries_sf)

groceries_sf$name

#Creating RDS files
for(e in 1:nrow(groceries_sf)){
 e=7
  grc_iso10 <- traveltime_map(appId= traveltime_id ,
                              apiKey = traveltime_api,
                              location=c(groceries_sf$lat[e], groceries_sf$long[e]),
                              traveltime=600,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(grc_iso10, file = paste0('grc_iso_10_',e,'.RDS'))
  grc_iso15 <- traveltime_map(appId= traveltime_id ,
                              apiKey = traveltime_api,
                              location=c(groceries_sf$lat[e], groceries_sf$long[e]),
                              traveltime=900,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(grc_iso15, file = paste0('grc_iso_15_',e,'.RDS'))
  residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
  m2 = mapview(grc_iso10, layer.name = "10 minute isochrone", col.regions = colors[1])
  m3 = mapview(grc_iso15, layer.name = "15 minute isochrone", col.regions = colors[2])
  # add the second layer on top
  m1 = m2 + m3 + residential
  mapshot(m1, file = paste0("~/git/dspg20patrick/output/isochrone_maps/grcmap_",e, ".png", sep = ""))
}

# wifi ------------------------------------------------------------------------
#read in csv file
wifi_full <- read_sf("./data/working/geocode/patrick_wifi.csv")

#adding wifi information for missing values via google maps
wifi_full$latitude[1] <- 36.7361785
wifi_full$longitude[1] <- -80.4008256

#Changing the long and lat data to numeric and removing NA
wifi <- wifi_full %>%
  mutate(lat = unlist(lapply(wifi_full$latitude,as.numeric)),
         long = unlist(lapply(wifi_full$longitude, as.numeric)))
#Changing the csv to sf and the CRS
wifi_sf <- st_as_sf(wifi, coords = c("longitude", "latitude"))

st_crs(wifi_sf) <- "+proj=longlat +datum=WGS84"

#Creating RDS files
for(w in 1:nrow(wifi_sf)){
  wifi_iso10 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(wifi_sf$lat[w], wifi_sf$long[w]),
                              traveltime=600,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(wifi_iso10, file = paste0('wifi_iso_10_',w,'.RDS'))
  wifi_iso15 <- traveltime_map(appId= traveltime_id,
                              apiKey = traveltime_api,
                              location=c(wifi_sf$lat[w], wifi_sf$long[w]),
                              traveltime=900,
                              type="driving",
                              departure="2020-08-07T08:00:00+01:00")
  saveRDS(wifi_iso15, file = paste0('wifi_iso_15_',w,'.RDS'))
  residential = mapview(st_geometry(residential_sf), cex =.5, layer.name = "residential areas", color = colors[5])
  m2 = mapview(wifi_iso10, layer.name = "10 minute isochrone", col.regions = colors[2])
  m3 = mapview(wifi_iso15, layer.name = "15 minute isochrone", col.regions = colors[3])
  # add the second layer on top
  m1 = m2 + m3 + residential
  mapshot(m1, file = paste0("~/git/dspg20patrick/output/isochrone_maps/wifimap_",w, ".png", sep = ""))
}

#crosswalks for  outputs in "~/git/dspg20patrick/output/isochrone_maps"
# emsmap_1.png -> STUART VOLUNTEER FIRE DEPARTMENT
# emsmap_2.png -> MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT
# emsmap_3.png -> BLUE RIDGE VOLUNTEER RESCUE SQUAD
# emsmap_4.png -> VESTA RESCUE SQUAD
# emsmap_5.png -> ARARAT RESCUE SQUAD
# emsmap_6.png -> FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 HQ
# emsmap_7.png -> JEB STUART RESCUE SQUAD
# emsmap_8.png -> SMITH RIVER RESCUE SQUAD
# emsmap_9.png -> FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2

# 1 <- "Mountain Meadow Farm and Craft Market"
# 2 <- "Lowes Foods of Stuart"                
# 3 <- "Patrick County Local Farmers Market"
# 4 <- "Stuart Farmers Market"                
# 5 <- "W & W Produce"
# 6 <- "Walmart Supercenter"                  
# 7 <- "Poor Farmers Farm" 
# I did not rerun the maps

# wifimap_1.png -> Meadows of Dan Elementary School
# wifimap_2.png -> Woolwine Elementary School
# wifimap_3.png -> Patrick Springs Primary School
# wifimap_4.png -> Blue Ridge Elementary School
# wifimap_5.png -> Patrick County High School
# wifimap_6.png -> Stuart Elementary School
# wifimap_7.png -> Patrick County Branch Library
# wifimap_8.png -> Hardin Reynolds Memorial School
# wifimap_9.png -> Stuart Baptist Church                      
# wifimap_10.png -> Patrick Henry Community College Stuart Campus
