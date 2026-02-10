con <- get_db_conn()

nk_allyears <- DBI::dbGetQuery(con, statement = paste(
  "SELECT fips_code, apn__parcel_number_unformatted_, apn_sequence_number, original_apn, census_tract, legal_lot_number, township, municipality_code,
  property_indicator_code, assessed_total_value, market_total_value, tax_amount, tax_year, assessed_year, 
  acres, land_square_footage, building_square_feet, living_square_feet, year_built, effective_year_built, 
  bedrooms, total_baths, full_baths, half_baths, parcel_level_latitude, parcel_level_longitude, block_level_latitude, block_level_longitude,
  situs_house_number, situs_street_name, situs_mode, situs_city, situs_state, situs_zip_code",
  "FROM corelogic_sdad.tax_hist_2_51",
  "WHERE fips_code='51127'"))

DBI::dbDisconnect(con)

nk_2018 <- nk_allyears[nk_allyears$tax_year == '2018',]
