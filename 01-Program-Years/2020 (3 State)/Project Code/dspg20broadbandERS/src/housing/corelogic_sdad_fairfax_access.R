# read in the CoreLogic data

# set up environmental vars (DON'T push actual values to github)
Sys.setenv(db_usr = "your_user_name")
Sys.setenv(db_pwd = "your_password")

get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"), # requires you to setup environmental vars (above)
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }

list_db_schemas <- function(db_con) {
  result <- DBI::dbGetQuery(db_con, "select schema_name from information_schema.schemata")
  DBI::dbDisconnect(db_con)
  return(result)
}

list_schema_tables <- function(db_con, db_schema) {
  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_name FROM information_schema.tables
                                           WHERE table_schema='", db_schema, "'"))
  DBI::dbDisconnect(db_con)
  return(result)
}

list_table_columns <- function(db_con, db_schema, db_table) {
  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_schema, table_name, column_name, data_type
                                           FROM information_schema.columns
                                           WHERE table_schema = '", db_schema, "'",
                                           " AND table_name = '", db_table, "'"))
  DBI::dbDisconnect(db_con)
  return(result)
}

# EXAMPLES ----
# List Schemas
con <- get_db_conn()
list_db_schemas(con)
# you should see CoreLogic data in this list

# List Tables in a Schema
con <- get_db_conn()
list_schema_tables(con, "corelogic_sdad")

# List Columns in a Table
con <- get_db_conn()
list_table_columns(con, "corelogic_sdad", "tax_hist_2_51")

# Get Some Data
con <- get_db_conn()
va_subset <- DBI::dbGetQuery(con, 'SELECT * FROM corelogic_sdad.tax_hist_2_51 LIMIT 200')
class(va_subset$fips_code)

# look at variable names
names(va_subset)

con <- get_db_conn()

ffx_allyears <- DBI::dbGetQuery(con, statement = paste(
  "SELECT fips_code, apn__parcel_number_unformatted_, apn_sequence_number, original_apn, census_tract, legal_lot_number, township, municipality_code,
  property_indicator_code, assessed_total_value, market_total_value, tax_amount, tax_year, assessed_year, 
  acres, land_square_footage, building_square_feet, living_square_feet, year_built, effective_year_built, 
  bedrooms, total_baths, full_baths, half_baths, parcel_level_latitude, parcel_level_longitude, block_level_latitude, block_level_longitude,
  situs_house_number, situs_street_name, situs_mode, situs_city, situs_state, situs_zip_code",
  "FROM corelogic_sdad.tax_hist_2_51",
  "WHERE fips_code='51059'"))

DBI::dbDisconnect(con)

#check if the assessed_year and the tax_year columns are identical
identical(ffx_allyears[[13]],ffx_allyears[[14]])

#hooray! they are (**IN THIS CASE FOR FAIRFAX** - need to check if applying other counties)

colnames(ffx_allyears)
sapply(ffx_allyears, class)

#filter by 2018
ffx_2018 <- ffx_allyears[ffx_allyears$tax_year == '2018',]
