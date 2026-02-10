library(tidyverse)

con <- get_db_conn()

va_allyears <- DBI::dbGetQuery(con, statement = paste(
  "SELECT fips_code, apn__parcel_number_unformatted_, apn_sequence_number, original_apn, census_tract, legal_lot_number, township, municipality_code,
  property_indicator_code, assessed_total_value, market_total_value, tax_amount, tax_year, assessed_year, 
  acres, land_square_footage, building_square_feet, living_square_feet, year_built, effective_year_built, 
  bedrooms, total_baths, full_baths, half_baths, parcel_level_latitude, parcel_level_longitude, block_level_latitude, block_level_longitude,
  situs_house_number, situs_street_name, situs_mode, situs_city, situs_state, situs_zip_code",
  "FROM corelogic_sdad.tax_hist_2_51",
  "WHERE state_fips='51'"))

DBI::dbDisconnect(con)

#filter by 2018
va_2018 <- va_allyears[va_allyears$tax_year == '2018',]

check_calc <- function(vec) {
  blanks <- 0L
  true_na <- 0L
  written_na <- 0L
  len <- length(x = vec)
  for (elem in vec) {
    if (is.na(x = elem)) {
      true_na <- true_na + 1L
    } else if (elem == "na") {
      written_na <- written_na + 1L
    } else if (elem == "") {
      blanks <- blanks + 1L
    }
  }
  percent_complete <- (len - (blanks + true_na + written_na)) / len
  unique_values <- length(unique(vec))
  tibble(blanks = blanks,
         true_na = true_na,
         written_na = written_na,
         percent_complete = percent_complete,
         unique_values = unique_values)
}
check_complete <- function(df) {
  z <- deparse(substitute(df))
  map_df(.x = df, .f = check_calc) %>%
    mutate(column = colnames(df)) %>%
    mutate(set = print(z))  %>%
    select(set, column, blanks, true_na, written_na, percent_complete, unique_values)
}

# census tract is 98.3 percent complete
check_complete(va_allyears)

# census tract is 93 percent complete
va2018completeness <- check_complete(va_2018)

va_no_census_tract <- filter(va_2018, is.na(census_tract))
# incomplete census tract entries are also very incomplete overall
check_complete(va_no_census_tract)

# read in virginia census geographies
va_census <- read_csv('~/git/dspg20broadbandERS/data/census-geographies/virginia_geography.csv')

# list of census tracts in virginia
va_tracts <- unique(va_census$tract)


#sampletracts <- va_tracts[1:3]

# function to save RDS object for each county
# subset_corelogic <- function(individual_tract, df){
#   temp <- filter(df, census_tract == individual_tract)
#   filename <- paste0('~/git/dspg20broadbandERS/data/virginia-corelogic',individual_tract)
#   saveRDS(temp, file = filename)
# }
# 
# sapply(sampletracts, subset_corelogic, df=va_2018)

for(i in 1:length(va_tracts)){
  temp <- va_2018[va_2018$census_tract==va_tracts[i],]
  filename <- paste0('~/git/dspg20broadbandERS/data/virginia-corelogic/', va_tracts[i])
  saveRDS(temp, file = filename)
}

va_2018_grouped <- va_2018 %>% 
  group_by(census_tract) %>%
  tally()

cltracts <- unique(va_2018_grouped$census_tract)

va_tracts_df <- as.data.frame(va_tracts)
