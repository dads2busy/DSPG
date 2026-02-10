# a fix to propertly join ACS and CoreLogic:
# we need to join by the 11 digits GEOID; state (2) + county (3) + tract (6)
# for the CoreLogic data, this means pasting together fips_code (5) and census_tract (6) before joining

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

# get GEOID by pasting fips_code and census_tract together
va_2018$GEOID <- paste0(va_2018$fips_code,va_2018$census_tract)

table(nchar(va_2018$fips_code), useNA="always")
#5    <NA> 
#2644505  159881

table(nchar(va_2018$census_tract), useNA="always")
#6       7      10    <NA> 
#124788   16702 2466073  196823 

table(nchar(va_2018$GEOID), useNA="always") # anything with 11 or more will join by GEOID
#4       7      11      12      15    <NA> 
#159881   36942  124788   16702 2466073     0

va_2018.tract <- va_2018
va_2018.tract$ct_detect <- substr(va_2018$GEOID,1,11)
va_2018.tract$ct_detect[ nchar(va_2018.tract$ct_detect) < 11] <- NA
table(nchar(va_2018.tract$ct_detect), useNA="always")
#11    <NA> 
#2607563  196823

#196,823 observations are missing census tracts in VA
sum(is.na(va_2018.tract$ct_detect))

library(tidycensus)
library(data.table)
library(maditr)
library(stringr)
library(dplyr)
library(tidyr)

census_api_key("853b2a1e71aa0aff0f3db966545e8898c73f0772")

acs_vars <- c("B25075_001", "B25075_002", "B25075_003", "B25075_004", "B25075_005", "B25075_006", "B25075_007", "B25075_008", "B25075_009", "B25075_010", "B25075_011", "B25075_012", "B25075_013", "B25075_014", "B25075_015", "B25075_016", "B25075_017", "B25075_018", "B25075_019", "B25075_020", "B25075_021", "B25075_022", "B25075_023", "B25075_024", "B25075_025", "B25075_026", "B25075_027", # value
              "B25077_001", # median value
              "B25001_001", # housing units
              #"B25002_001", #occ status total
              "B25002_002", "B25002_003",# occupancy status
              #"B25024_001", "B25024_002", "B25024_003", "B25024_004", "B25024_005", "B25024_006", "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25024_011", # number of units
              #"B25041_001", "B25041_002", "B25041_003", "B25041_004", "B25041_005", "B25041_006", "B25041_007", # number of bedrooms
              "B25034_001", "B25034_002", "B25034_003", "B25034_004", "B25034_005", "B25034_006", "B25034_007", "B25034_008", "B25034_009", "B25034_010", "B25034_011", # year built
              "B25035_001" # median year built
)

# function to clean raw ACS data
# changes to a tidy format that works well with dplyr & ggplot
# adds descriptive variable names
# calculates and stores 90% confidence interval boundaries
get_estimates_tidy <- function(acs_data) {
  acs_data <- as.data.table(acs_data) %>%
    dt_mutate(NAME = str_extract(NAME, "(?<=Census Tract )[0-9.]*")) %>%
    rename(census_tract = NAME) %>%
    dt_mutate(variable = str_replace(variable, "B25077_001", "value median")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_001", "value total")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_002", "value <10")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_003", "value 10-15")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_004", "value 15-20")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_005", "value 20-25")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_006", "value 25-30")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_007", "value 30-35")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_008", "value 35-40")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_009", "value 40-50")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_010", "value 50-60")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_011", "value 60-70")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_012", "value 70-80")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_013", "value 80-90")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_014", "value 90-100")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_015", "value 100-125")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_016", "value 125-150")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_017", "value 150-175")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_018", "value 175-200")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_019", "value 200-250")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_020", "value 250-300")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_021", "value 300-400")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_022", "value 400-500")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_023", "value 500-750")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_024", "value 750-1000")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_025", "value 1000-1500")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_026", "value 1500-2000")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_027", "value >2000")) %>%
    dt_mutate(variable = str_replace(variable, "B25035_001", "yrbuilt median")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_001", "yrbuilt total")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_002", "yrbuilt 2014-later")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_003", "yrbuilt 2010-2013")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_004", "yrbuilt 2000-2009")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_005", "yrbuilt 1990-1999")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_006", "yrbuilt 1980-1989")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_007", "yrbuilt 1970-1979")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_008", "yrbuilt 1960-1969")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_009", "yrbuilt 1950-1959")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_010", "yrbuilt 1940-1949")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_011", "yrbuilt earlier-1939")) %>%
    #dt_mutate(variable = str_replace(variable, "B25041_001", "bedrooms total")) %>%
    #dt_mutate(variable = str_replace(variable, "B25041_002", "bedrooms none")) %>%
    #dt_mutate(variable = str_replace(variable, "B25041_003", "bedrooms 1")) %>%
    #dt_mutate(variable = str_replace(variable, "B25041_004", "bedrooms 2")) %>%
    #dt_mutate(variable = str_replace(variable, "B25041_005", "bedrooms 3")) %>%
    #dt_mutate(variable = str_replace(variable, "B25041_006", "bedrooms 4")) %>%
    #dt_mutate(variable = str_replace(variable, "B25041_007", "bedrooms 5+")) %>%
    dt_mutate(variable = str_replace(variable, "B25001_001", "housing_units total")) %>%
    #dt_mutate(variable = str_replace(variable, "B25002_001", "occupancy_status total")) %>%
    dt_mutate(variable = str_replace(variable, "B25002_002", "occupancy_status occupied")) %>%
    dt_mutate(variable = str_replace(variable, "B25002_003", "occupancy_status vacant")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_001", "unit_no total")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_002", "unit_no 1 detached")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_003", "unit_no 1 attached")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_004", "unit_no 2")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_005", "unit_no 3-4")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_006", "unit_no 5-9")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_007", "unit_no 10-19")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_008", "unit_no 20-49")) %>%
  #dt_mutate(variable = str_replace(variable, "B25024_009", "unit_no 50+")) %>%
  #dt_mutate(variable = str_replace(variable, "B25024_010", "unit_no mobile")) %>%
  #dt_mutate(variable = str_replace(variable, "B25024_011", "unit_no other")) %>%
  dt_mutate(value = str_extract(variable, "(?<=\\s).+")) %>%
    dt_mutate(variable = str_extract(variable, "[a-z_]+")) %>%
    setcolorder(c("GEOID", "census_tract", "variable", "value", "estimate", "moe")) %>%
    dt_mutate(lower_ci = estimate - moe) %>%
    dt_mutate(upper_ci = estimate + moe) %>%
    dt_mutate(lower_ci = if_else(condition = lower_ci < 0, true = 0, false = lower_ci))
}

get_estimates_tidy2 <- function(acs_data) {
  acs_data <- as.data.table(acs_data) %>%
    dt_mutate(NAME = str_extract(NAME, "(?<=Census Tract )[0-9.]*")) %>%
    rename(census_tract = NAME) %>%
    dt_mutate(variable = str_replace(variable, "B25077_001", "value median")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_001", "value total")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_002", "value less10")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_003", "value 10.15")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_004", "value 15.20")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_005", "value 20.25")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_006", "value 25.30")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_007", "value 30.35")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_008", "value 35.40")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_009", "value 40.50")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_010", "value 50.60")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_011", "value 60.70")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_012", "value 70.80")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_013", "value 80.90")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_014", "value 90.100")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_015", "value 100.125")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_016", "value 125.150")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_017", "value 150.175")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_018", "value 175.200")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_019", "value 200.250")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_020", "value 250.300")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_021", "value 300.400")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_022", "value 400.500")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_023", "value 500.750")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_024", "value 750.1000")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_025", "value 1000.1500")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_026", "value 1500.2000")) %>%
    dt_mutate(variable = str_replace(variable, "B25075_027", "value 2000plus")) %>%
    dt_mutate(variable = str_replace(variable, "B25035_001", "yrbuilt median")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_001", "yrbuilt total")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_002", "yrbuilt 2014.plus")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_003", "yrbuilt 2010.2013")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_004", "yrbuilt 2000.2009")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_005", "yrbuilt 1990.1999")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_006", "yrbuilt 1980.1989")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_007", "yrbuilt 1970.1979")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_008", "yrbuilt 1960.1969")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_009", "yrbuilt 1950.1959")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_010", "yrbuilt 1940.1949")) %>%
    dt_mutate(variable = str_replace(variable, "B25034_011", "yrbuilt earlier.1939")) %>%
    #dt_mutate(variable = str_replace(variable, "B25041_001", "bedrooms total")) %>%
    #dt_mutate(variable = str_replace(variable, "B25041_002", "bedrooms none")) %>%
    dt_mutate(variable = str_replace(variable, "B25041_003", "bedrooms 1")) %>%
    dt_mutate(variable = str_replace(variable, "B25041_004", "bedrooms 2")) %>%
    dt_mutate(variable = str_replace(variable, "B25041_005", "bedrooms 3")) %>%
    dt_mutate(variable = str_replace(variable, "B25041_006", "bedrooms 4")) %>%
    dt_mutate(variable = str_replace(variable, "B25041_007", "bedrooms 5plus")) %>%
    dt_mutate(variable = str_replace(variable, "B25001_001", "housing_units total")) %>%
    #dt_mutate(variable = str_replace(variable, "B25002_001", "occupancy_status total")) %>%
    dt_mutate(variable = str_replace(variable, "B25002_002", "occupancy_status occupied")) %>%
    dt_mutate(variable = str_replace(variable, "B25002_003", "occupancy_status vacant")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_001", "unit_no total")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_002", "unit_no 1 detached")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_003", "unit_no 1 attached")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_004", "unit_no 2")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_005", "unit_no 3-4")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_006", "unit_no 5-9")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_007", "unit_no 10-19")) %>%
    #dt_mutate(variable = str_replace(variable, "B25024_008", "unit_no 20-49")) %>%
  #dt_mutate(variable = str_replace(variable, "B25024_009", "unit_no 50+")) %>%
  #dt_mutate(variable = str_replace(variable, "B25024_010", "unit_no mobile")) %>%
  #dt_mutate(variable = str_replace(variable, "B25024_011", "unit_no other")) %>%
  dt_mutate(value = str_extract(variable, "(?<=\\s).+")) %>%
    dt_mutate(variable = str_extract(variable, "[a-z_]+")) %>%
    setcolorder(c("GEOID", "census_tract", "variable", "value", "estimate", "moe")) %>%
    dt_mutate(lower_ci = estimate - moe) %>%
    dt_mutate(upper_ci = estimate + moe) %>%
    dt_mutate(lower_ci = if_else(condition = lower_ci < 0, true = 0, false = lower_ci))
}

#using Morgan's acs_vars and get_estimates_tidy functions here - may need to add in
va_acs <- get_acs(geography = "tract", state = 51,
                  variables = acs_vars,
                  year = 2018, survey = "acs5",
                  cache_table = TRUE)

va_acs_estimates <- get_estimates_tidy2(va_acs)

# smush together a column, extract the 6 digit census tract code
va_acs_estimates <- va_acs_estimates %>%
  unite(varval, variable:value)

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
            cl_value_175.200 = sum(cl_value_175.200, na.rm = TRUE),
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
#va_2018_cl_mergeable <- va_2018_cl_collapsed %>%
#  gather(key = 'cl_tract', value = 'cl_value', 2:50)

# -------------------------------------------------
# join ACS and CL by tract

length( unique(va_acs_estimates$GEOID) )
# 1907 tracts in ACS data
length( unique(va_2018_cl_collapsed$ct_detect) )
# but only 1386 tracts in CoreLogic data

# spread ACS data (estimate, lower CI, upper CI separately)
va_acs_wide_estimate <- va_acs_estimates %>% select(GEOID, varval, estimate) %>% spread(key=varval, value=estimate)

va_acs_wide_lower_ci <- va_acs_estimates %>% select(GEOID, varval, lower_ci) %>% spread(key=varval, value=lower_ci)
names(va_acs_wide_lower_ci)[2:42] <- paste0(names(va_acs_wide_lower_ci)[2:42],"_lower")

va_acs_wide_upper_ci <- va_acs_estimates %>% select(GEOID, varval, upper_ci) %>% spread(key=varval, value=upper_ci)
names(va_acs_wide_upper_ci)[2:42] <- paste0(names(va_acs_wide_upper_ci)[2:42],"_upper")

va_acs_wide_moe <- va_acs_estimates %>% select(GEOID, varval, moe) %>% spread(key=varval, value=moe)
names(va_acs_wide_moe)[2:42] <- paste0(names(va_acs_wide_moe)[2:42],"_moe")

# join them together
acs_joined <- va_acs_wide_estimate %>% left_join(va_acs_wide_lower_ci, by="GEOID")
acs_joined2 <- acs_joined %>% left_join(va_acs_wide_upper_ci, by="GEOID")
acs_joined3 <- acs_joined2 %>% left_join(va_acs_wide_moe, by="GEOID")

# now join CoreLogic; GEOID to ct_detect
acs_cl_joined <- acs_joined3 %>% left_join(va_2018_cl_collapsed, by=c("GEOID"="ct_detect"))

write_csv(acs_cl_joined, "~/git/dspg20broadbandERS/data/acs-cl-joined/acs_cl_joined_va.csv")
