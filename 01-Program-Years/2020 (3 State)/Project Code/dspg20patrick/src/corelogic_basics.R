library(RPostgreSQL)
library(dplyr)
library(readr)
library(sf)


#
# Connect to DB ---------------------------------------------------------------------------------------------
#

conn <- dbConnect(drv = PostgreSQL(), dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv(x = "db_userid"), 
                  password = Sys.getenv(x = "db_pwd"))


#
# Get Patrick County data from the available tables-----------------------------------------------------------
#

# Most info: Get data from _2_51 files ("latest tax data + property characteristics" files split by state, 51 is VA), PC VA FIPS is 51141
patrick <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_2_51 WHERE fips_code = '51141'") # 21,416 rows, 199 vars

# Less info:
# Get data from _1_51 files ("latest tax data" files split by state, 51 is VA), PC VA FIPS is 51141
# patrick <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_1_51 WHERE fips_code = '51141'") # 21,422, 149 vars

# Sanity check:
# Get data from the 01-09 files (dump split into tables); Fairfax County, PC VA FIPS is 51141
# p1 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_01 WHERE fips_code = '51141'") # 21,422
# p2 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_02 WHERE fips_code = '51141'") # 21,329
# p3 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_03 WHERE fips_code = '51141'") # 21,255
# p4 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_04 WHERE fips_code = '51141'") # 21,221
# p5 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_05 WHERE fips_code = '51141'") # 21,213
# p6 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_06 WHERE fips_code = '51141'") # 21,204
# p7 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_07 WHERE fips_code = '51141'") # 21,065
# p8 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_08 WHERE fips_code = '51141'") # 21,077
# p9 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_09 WHERE fips_code = '51141'") # 20,693


#
# Disconnect ---------------------------------------------------------------------------------------------
#

dbDisconnect(conn)
rm(conn)


#
# Check contents ---------------------------------------------------------------------------------------------
#

names(patrick)

# Year
table(patrick$tax_year, useNA = "always")          # 2019
table(patrick$assessed_year, useNA = "always")     # 2019

table(p1$tax_year, useNA = "always")          # 2018
table(p1$assessed_year, useNA = "always")     # 2018

table(p2$tax_year, useNA = "always")          # 2017
table(p2$assessed_year, useNA = "always")     # 2017

# Geo identifiers
# empty: block_level_latitude, block_level_longitude, 
# usable: parcel_level_latitude, parcel_level_longitude, census_tract

# Other information not NA:
# land_use_code, county_use_description, property_indicator_code
# owner_1_full_name, owner_1_last_name, owner_1_corporate_indicator, 
# total_value_calculated


#
# Check ---------------------------------------------------------------------------------------------
#

table(patrick$land_use_code, useNA = "always")
#133   163   200   211   500   600   601   602   604   614   650   675 NA
#28 16706   128   289  3310     9   490    75   107    16    28   230   0

table(patrick$owner_1_corporate_indicator, useNA = "always")
# Y     <NA> 
# 2521 18895 

table(patrick$county_use_description, useNA = "always")
# AGRICULTURAL 100 OR MORE ACRES       AGRICULTURAL 20-100 ACRE     COMMERCIAL AND RESIDENTIAL          EDUCATIONAL 
# 392                                  2918                            417                             28 
# FEDERAL GOVERNMENT               LOCAL GOVERNMENT       MULTI FAMILY RESIDENTIAL            MULTIPLE GOVERNMENT 
# 16                                107                             28                              5 
# OTHER            REGIONAL GOVERNMENT                      RELIGIOUS  SINGLE FAM RESIDENTIAL-SUBURB 
# 490                              4                            230                          15916 
# SINGLE FAM RESIDENTIAL-URBAN               STATE GOVERNMENT                           <NA> 
#   790                                                   75                              0 

# Residential: 28 + 15916 + 790 = 16734; residential with "commercial and residential) = 17151


#
# Clean residential & plot ---------------------------------------------------------------------------------------------
#

sum(is.na(patrick$parcel_level_longitude)) # 344
sum(is.na(patrick$parcel_level_latitude)) # 344

residential <- patrick %>% filter(!is.na(parcel_level_longitude) &
                                  !is.na(parcel_level_latitude))

residential <- residential %>% filter(county_use_description == "COMMERCIAL AND RESIDENTIAL" |
                                  county_use_description == "MULTI FAMILY RESIDENTIAL" |
                                  county_use_description == "SINGLE FAM RESIDENTIAL-SUBURB" |
                                  county_use_description == "SINGLE FAM RESIDENTIAL-URBAN")

# Test plot
residential_sf <- st_as_sf(residential, coords = c("parcel_level_longitude", "parcel_level_latitude"))
plot(st_geometry(residential_sf), pch = 21, cex = 0.3)


#
# Write out ---------------------------------------------------------------------------------------------
#

write.csv(residential, "./data/working/corelogic/residential.csv")

residential <- residential %>% select(
  composite_property_linkage_key, county_use_description, land_use_code,
  parcel_level_latitude, parcel_level_longitude)

residential <- residential %>% rename(latitude = parcel_level_latitude, longitude = parcel_level_longitude)
write_rds(residential, "./data/web/residential.Rds")


