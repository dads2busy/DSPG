library(maps)
library(tigris)
# continental + AK, HI state fips
state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)

# only VA FIPS
state_fips <- '51'
va_tracts <- list()
for(i in 1:length(state_fips)){
  va_tracts[[i]] <- tracts(state=state_fips[i], year=2018)
}

va_tracts <- as.data.frame(va_tracts)

# get regex of all census tract codes
regex_census_tract <- paste0(va_tracts$TRACTCE, collapse = "|")

# need to load va_2018 data 
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

con <- get_db_conn()

va_allyears <- DBI::dbGetQuery(con, statement = paste(
  "SELECT fips_code, apn__parcel_number_unformatted_, apn_sequence_number, original_apn, census_tract, legal_lot_number, township, municipality_code,
  property_indicator_code, assessed_total_value, market_total_value, tax_year, assessed_year, 
  building_square_feet, living_square_feet, year_built, 
  bedrooms, total_baths, full_baths, half_baths",
  "FROM corelogic_sdad.tax_hist_2_51",
  "WHERE state_fips='51'"))

DBI::dbDisconnect(con)

#filter by 2018
va_2018 <- va_allyears[va_allyears$tax_year == '2018',]

# create new column to join on using regex
va_2018.tract <- va_2018 %>% 
  mutate(ct_detect = str_extract_all(census_tract, pattern = regex_census_tract))

#196,823 observations are missing census tracts in VA
sum(is.na(va_2018.tract$ct_detect))

#using Morgan's acs_vars and get_estimates_tidy functions here - may need to add in
va_acs <- get_acs(geography = "tract", state = 51,
                  variables = acs_vars,
                  year = 2018, survey = "acs5",
                  cache_table = TRUE)

va_acs_estimates <- get_estimates_tidy(va_acs)

# smush together a column, extract the 6 digit census tract code
va_acs_estimates <- va_acs_estimates %>%
  unite(varval, variable:value) %>%
  mutate(census_tract_six = substr(as.character(GEOID), 6,11))

#118234 rows in acs estimates
nrow(va_acs_estimates)

#2804386 rows in va cl data
nrow(va_2018.tract)

va_2018.tract$ct_detect <- as.character(va_2018.tract$ct_detect)

sapply(va_2018.tract, class)

# time to collapse the corelogic data
va_2018_cl_categories <- va_2018.tract %>%
  mutate(assessed_total_value = as.numeric(assessed_total_value),
         bedrooms = as.numeric(bedrooms),
         year_built = as.numeric(year_built),
         property_indicator_code = as.numeric(property_indicator_code),
         cl_value_less10 = ifelse(assessed_total_value <10000, 1, 0),
         cl_value_10.15 = ifelse(assessed_total_value > 10000 & assessed_total_value <= 15000, 1, 0),
         cl_value_15.20 = ifelse(assessed_total_value > 15000 & assessed_total_value <= 20000, 1, 0),
         cl_value_20.25 = ifelse(assessed_total_value > 20000 & assessed_total_value <= 25000, 1, 0),
         cl_value_25.30 = ifelse(assessed_total_value > 25000 & assessed_total_value <= 30000, 1, 0),
         cl_value_30.35 = ifelse(assessed_total_value > 30000 & assessed_total_value <= 35000, 1, 0),
         cl_value_35.40 = ifelse(assessed_total_value > 35000 & assessed_total_value <= 40000, 1, 0),
         cl_value_40.50 = ifelse(assessed_total_value > 40000 & assessed_total_value <= 50000, 1, 0),
         cl_value_50.60 = ifelse(assessed_total_value > 50000 & assessed_total_value <= 60000, 1, 0),
         cl_value_60.70 = ifelse(assessed_total_value > 60000 & assessed_total_value <= 70000, 1, 0),
         cl_value_70.80 = ifelse(assessed_total_value > 70000 & assessed_total_value <= 80000, 1, 0),
         cl_value_80.90 = ifelse(assessed_total_value > 80000 & assessed_total_value <= 90000, 1, 0),
         cl_value_90.100 = ifelse(assessed_total_value > 90000 & assessed_total_value <= 100000, 1, 0),
         cl_value_100.125 = ifelse(assessed_total_value > 100000 & assessed_total_value <= 125000, 1, 0),
         cl_value_125.150 = ifelse(assessed_total_value > 125000 & assessed_total_value <= 150000, 1, 0),
         cl_value_150.175 = ifelse(assessed_total_value > 150000 & assessed_total_value <= 175000, 1, 0),
         cl_value_175.200 = ifelse(assessed_total_value > 175000 & assessed_total_value <= 200000, 1, 0),
         cl_value_200.250 = ifelse(assessed_total_value > 200000 & assessed_total_value <= 250000, 1, 0),
         cl_value_250.300 = ifelse(assessed_total_value > 250000 & assessed_total_value <= 300000, 1, 0),
         cl_value_300.400 = ifelse(assessed_total_value > 300000 & assessed_total_value <= 400000, 1, 0),
         cl_value_400.500 = ifelse(assessed_total_value > 400000 & assessed_total_value <= 500000, 1, 0),
         cl_value_500.750 = ifelse(assessed_total_value > 500000 & assessed_total_value <= 750000, 1, 0),
         cl_value_750.1000 = ifelse(assessed_total_value > 750000 & assessed_total_value <= 1000000, 1, 0),
         cl_value_1000.1500 = ifelse(assessed_total_value > 1000000 & assessed_total_value <= 1500000, 1, 0),
         cl_value_1500.2000 = ifelse(assessed_total_value > 1500000 & assessed_total_value <= 2000000, 1, 0),
         cl_value_2000plus = ifelse(assessed_total_value > 2000000, 1, 0),
         cl_bedrooms_none = ifelse(bedrooms == 0, 1, 0),
         cl_bedrooms_1 = ifelse(bedrooms == 1, 1, 0),
         cl_bedrooms_2 = ifelse(bedrooms == 2, 1, 0),
         cl_bedrooms_3 = ifelse(bedrooms == 3, 1, 0),
         cl_bedrooms_4 = ifelse(bedrooms == 4, 1, 0),
         cl_bedrooms_5plus = ifelse(bedrooms > 4, 1, 0),
         cl_yrbuilt_2014plus = ifelse(year_built >= 2014, 1, 0),
         cl_yrbuilt_2010.2013 = ifelse(year_built > 2010 & year_built <= 2013, 1, 0),
         cl_yrbuilt_2000.2009 = ifelse(year_built > 2000 & year_built <= 2009, 1, 0),
         cl_yrbuilt_1990.1999 = ifelse(year_built > 1990 & year_built <= 1999, 1, 0),
         cl_yrbuilt_1980.1989 = ifelse(year_built > 1980 & year_built <= 1989, 1, 0),
         cl_yrbuilt_1970.1979 = ifelse(year_built > 1970 & year_built <= 1979, 1, 0),
         cl_yrbuilt_1960.1969 = ifelse(year_built > 1960 & year_built <= 1969, 1, 0),
         cl_yrbuilt_1950.1959 = ifelse(year_built > 1950 & year_built <= 1959, 1, 0),
         cl_yrbuilt_1940.1949 = ifelse(year_built > 1940 & year_built <= 1949, 1, 0),
         cl_yrbuilt_1939less = ifelse(year_built <= 1939, 1, 0),
         cl_vacant = ifelse(property_indicator_code == 90, 1, 0),
         cl_occupied = ifelse((property_indicator_code == 10 | property_indicator_code == 11 | property_indicator_code == 21 | property_indicator_code == 22 | property_indicator_code == 24), 1, 0))

va_2018_cl_collapsed <- va_2018_cl_categories %>%
  filter(!is.na(ct_detect)) %>%
  group_by(ct_detect) %>%
  summarize(cl_value_median = median(assessed_total_value, na.rm=TRUE),
            cl_value_total = sum(assessed_total_value, na.rm = TRUE),
            cl_value_less10 = sum(cl_value_less10, na.rm = TRUE),
            cl_value_10.15 = sum(cl_value_10.15, na.rm = TRUE),
            cl_value_15.20 = sum(cl_value_15.20, na.rm = TRUE),
            cl_value_20.25 = sum(cl_value_20.25, na.rm = TRUE),
            cl_value_25.30 = sum(cl_value_25.30, na.rm = TRUE),
            cl_value_30.35 = sum(cl_value_30.35, na.rm = TRUE),
            cl_value_35.40 = sum(cl_value_35.40, na.rm = TRUE),
            cl_value_40.50 = sum(cl_value_40.50, na.rm = TRUE),
            cl_value_50.60 = sum(cl_value_50.60, na.rm = TRUE),
            cl_value_60.70 = sum(cl_value_60.70, na.rm = TRUE),
            cl_value_70.80 = sum(cl_value_70.80, na.rm = TRUE),
            cl_value_80.90 = sum(cl_value_80.90, na.rm = TRUE),
            cl_value_90.100 = sum(cl_value_90.100, na.rm = TRUE),
            cl_value_100.125 = sum(cl_value_100.125, na.rm = TRUE),
            cl_value_125.150 = sum(cl_value_125.150, na.rm = TRUE),
            cl_value_150.175 = sum(cl_value_150.175, na.rm = TRUE),
            cl_value_200.250 = sum(cl_value_200.250, na.rm = TRUE),
            cl_value_250.300 = sum(cl_value_250.300, na.rm = TRUE),
            cl_value_300.400 = sum(cl_value_300.400, na.rm = TRUE),
            cl_value_400.500 = sum(cl_value_400.500, na.rm = TRUE),
            cl_value_500.750 = sum(cl_value_500.750, na.rm = TRUE),
            cl_value_750.1000 = sum(cl_value_750.1000, na.rm = TRUE),
            cl_value_1000.1500 = sum(cl_value_1000.1500, na.rm = TRUE),
            cl_value_1500.2000 = sum(cl_value_1500.2000, na.rm = TRUE),
            cl_value_2000plus = sum(cl_value_2000plus, na.rm = TRUE),
            cl_bedrooms_none = sum(cl_bedrooms_none, na.rm = TRUE),
            cl_bedrooms_1 = sum(cl_bedrooms_1, na.rm = TRUE),
            cl_bedrooms_2 = sum(cl_bedrooms_2, na.rm = TRUE),
            cl_bedrooms_3 = sum(cl_bedrooms_3, na.rm = TRUE),
            cl_bedrooms_4 = sum(cl_bedrooms_4, na.rm = TRUE),
            cl_bedrooms_5plus = sum(cl_bedrooms_5plus, na.rm = TRUE),
            cl_bedrooms_total = sum(bedrooms, na.rm = TRUE),
            cl_yrbuilt_1939less = sum(cl_yrbuilt_1939less, na.rm = TRUE),
            cl_yrbuilt_1940.1949 = sum(cl_yrbuilt_1940.1949, na.rm = TRUE),
            cl_yrbuilt_1950.1959 = sum(cl_yrbuilt_1950.1959, na.rm = TRUE),
            cl_yrbuilt_1960.1969 = sum(cl_yrbuilt_1960.1969, na.rm = TRUE),
            cl_yrbuilt_1970.1979 = sum(cl_yrbuilt_1970.1979, na.rm = TRUE),
            cl_yrbuilt_1980.1989 = sum(cl_yrbuilt_1980.1989, na.rm = TRUE),
            cl_yrbuilt_1990.1999 = sum(cl_yrbuilt_1990.1999, na.rm = TRUE),
            cl_yrbuilt_2000.2009 = sum(cl_yrbuilt_2000.2009, na.rm = TRUE),
            cl_yrbuilt_2010.2013 = sum(cl_yrbuilt_2010.2013, na.rm = TRUE),
            cl_yrbuilt_2014plus = sum(cl_yrbuilt_2014plus, na.rm = TRUE),
            cl_yrbuilt_median = median(year_built, na.rm = TRUE),
            cl_yrbuilt_total = sum(year_built, na.rm = TRUE),
            cl_vacant = sum(cl_vacant, na.rm = TRUE),
            cl_occupied = sum(cl_occupied, na.rm = TRUE), # based on residential vars
            cl_occstatus_total = (cl_vacant + cl_occupied)
            )

# wide to long
va_2018_cl_mergeable <- va_2018_cl_collapsed %>%
  gather(key = 'cl_tract', value = 'cl_value', 2:50)


                        