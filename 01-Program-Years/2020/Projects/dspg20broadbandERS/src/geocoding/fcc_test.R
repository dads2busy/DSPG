# LOAD LIBRARIES
library(dataplumbr)
library(RPostgreSQL)
library(data.table)

# LOAD FUNCTIONS
functions <- list.files("src/functions/", full.names = TRUE)
for (f in functions) source(f)

sql <- "select unformatted_apn, situs_house_number, census_tract, block_number, bldg_code, property_centroid_longitude, property_centroid_latitude, block_level_latitude, block_level_longitude from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code = '51710' and situs_house_number != '' and block_level_latitude IS NOT NULL limit 10"

sql <- "select fips, apn_parcel_number_unformatted, pcl_id_iris_formatted, property_level_latitude, property_level_longitude from corelogic_usda.corelogic_usda_deed_2020_06_27 where fips = '51710' and apn_parcel_number_unformatted != '' and property_indicator = '\"10\"' limit 10" 

con <- get_db_conn()
 db_rows <- setDT(dbGetQuery(con, sql))
dbDisconnect(con)

system.time(ute <- setDT(loc.lats_lons2geo_areas(place_idCol = db_rows$apn_parcel_number_unformatted, latCol = db_rows$property_level_latitude, lonCol = db_rows$property_level_longitude), keep.rownames = T))

mrg <- merge(db_rows, ute, by.x = "unformatted_apn", by.y = "rn")

loc.lat_lon2geo_areas(lon = -77.11431, lat = 38.88301)
