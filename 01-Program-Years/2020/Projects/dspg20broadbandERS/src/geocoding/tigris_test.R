library(RPostgreSQL)

con <- get_db_conn()
arl_houses <- dbGetQuery(con, "select situs_house_number, situs_house_number_suffix, situs_direction, situs_street_name, situs_mode, situs_city, situs_state,
                         property_centroid_longitude lat, property_centroid_latitude lon
                         from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = '51013'")
dbDisconnect(con)

arl_houses_unq <- unique(arl_houses[arl_houses$situs_house_number != "" & !is.na(arl_houses$situs_house_number),])

samp_1 <- arl_houses_unq[1:1000,]
samp_1$street <- paste(samp_1$situs_house_number, samp_1$situs_direction, samp_1$situs_street_name, samp_1$situs_mode)
samp_1$city <- samp_1$situs_city
samp_1$state <- samp_1$situs_state
#samp_1$row <- seq(2, 10, 1)

samp_2 <- tigris::append_geoid(samp_1[, c("street", "city", "state")], "block")

samp_1$lat <- as.numeric(samp_1$lat)
samp_1$lon <- as.numeric(samp_1$lon)

tigris::append_geoid(samp_1[, c("lat", "lon")], "block group")

con <- get_db_conn()
arl_houses_lat_lon <- dbGetQuery(con, "select property_centroid_latitude lat, property_centroid_longitude lon from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = '51013'")
dbDisconnect(con)

arl_houses_lat_lon_unq <- unique(arl_houses_lat_lon)
arl_houses_lat_lon_unq <- arl_houses_lat_lon_unq[is.na(arl_houses_lat_lon_unq$lat) == FALSE,]
arl_houses_lat_lon_unq$lat <- as.numeric(arl_houses_lat_lon_unq$lat)
arl_houses_lat_lon_unq$lon <- as.numeric(arl_houses_lat_lon_unq$lon)
colnames(arl_houses_lat_lon_unq) <- c("lon", "lat")



tigris::append_geoid(arl_houses_lat_lon_unq[1:20, c("lat", "lon")])


dataplumbr::loc.lats_lons2geo_areas(samp_1$row, lonCol = samp_1$lon, latCol = samp_1$lat)


library(sf)
v <- st_read("data/virginia/VirginiaParcel.shp")



