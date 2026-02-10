con <- get_db_conn()

ffx <- DBI::dbGetQuery(con, statement = paste(
  
  "SELECT  situs_house_number, situs_street_name, situs_mode, situs_city, situs_state, situs_zip_code, tax_year",
  
  "FROM corelogic_sdad.tax_hist_2_51",
  
  "WHERE fips_code='51059'"))

ffx_2018 <- ffx[ffx$tax_year == '2018',]

ffx_2018$addr <- paste0(ffx$situs_house_number, " ",
                   
                   ffx$situs_street_name, " ",
                   
                   ffx$situs_mode, ", ",
                   
                   ffx$situs_city, ", ", 
                   
                   ffx$situs_state, " ",
                   
                   ffx$situs_zip_code)

ffx_2018$addr

library(tidygeocoder)

library(dplyr)

ffx_latlon_na <- filter(ffx_2018, is.na(parcel_level_latitude) | is.na(parcel_level_longitude))

ffx_2018 %>% distinct()

ffx_geo_census <- tidygeocoder::geocode(ffx_latlon_na, addr, method = "census")

#ffx_geo_census_na <- filter(ffx_geo_census, is.na(lat) | is.na(long))
# 
# ffx_geo_census_false_na <- filter(ffx_geo_census, (grepl("NA ", addr)) & (is.na(lat) & is.na(long)))
# 
# ffx_geo_census_true_na <- filter(ffx_geo_census, !(grepl("NA ", addr)) & (is.na(lat) & is.na(long)))
# 
# ffx_geo_osm <- tidygeocoder::geocode(ffx_geo_census_true_na, addr, method = "osm")


table(is.na(ffx_geo_census$lat))
