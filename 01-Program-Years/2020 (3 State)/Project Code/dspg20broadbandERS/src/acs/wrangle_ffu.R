library(tidyverse)

# read county level data
va_table_wide <- read_csv('~/git/dspg20broadbandERS/data/acs-cl-joined/va_table_countylevel.csv')
va_table_wide <- va_table_wide %>%
  gather('variable', 'value', 2:41) %>%
  mutate(variable = recode_factor(variable, 
                           'percent_rural' = 'Percent Rural',
                           'mean_ffu_housing_units_total' = 'Total Housing Units',
                           'mean_occ_status_occupied' = 'Total Occupied Units',
                           'mean_occ_status_vacant' = 'Total Vacant Units',
                           'mean_value_less10' = 'Value Below $10,000',
                           'mean_value_10.15' = 'Value $10,000-$15,000',
                           'mean_value_15.20' = 'Value $15,000-$20,000',
                           'mean_value_20.25' = 'Value $20,000-$25,000',
                           'mean_value_25.30' = 'Value $25,000-$30,000',
                           'mean_value_30.35' = 'Value $30,000-$35,000',
                           'mean_value_35.40' = 'Value $35,000-$40,000',
                           'mean_value_40.50' = 'Value $40,000-$50,000',
                           'mean_value_50.60' = 'Value $50,000-$60,000',
                           'mean_value_60.70' = 'Value $60,000-$70,000',
                           'mean_value_70.80' = 'Value $70,000-$80,000',
                           'mean_value_80.90' = 'Value $80,000-$90,000',
                           'mean_value_90.100' = 'Value $90,000-$100,000',
                           'mean_value_100.125' = 'Value $100,000-$125,000',
                           'mean_value_125.150' = 'Value $125,000-$150,000',
                           'mean_value_150.175' = 'Value $150,000-175,000',
                           'mean_value_175.200' = 'Value $175,000-$200,000',
                           'mean_value_200.250' = 'Value $200,000-$250,000',
                           'mean_value_250.300' = 'Value $250,000-$300,000',
                           'mean_value_300.400' = 'Value $300,000-$400,000',
                           'mean_value_400.500' = 'Value $400,000-$500,000',
                           'mean_value_500.750' = 'Value $500,000-$750,000',
                           'mean_value_750.1000' = 'Value $750,000-$1,000,000',
                           'mean_value_1000.1500' = 'Value $1,000,000-$1,500,000',
                           'mean_value_1500.2000' = 'Value $1,500,000-$2,000,000',
                           'mean_value_2000plus' = 'Value Over $2,000,000',
                           'mean_yrbuilt_1939less' = 'Built Before 1939',
                           'mean_yrbuilt_1940.1949' = 'Built 1940-1949',
                           'mean_yrbuilt_1950.1959' = 'Built 1950-1959',
                           'mean_yrbuilt_1960.1969' = 'Built 1960-1969',
                           'mean_yrbuilt_1970.1979' = 'Built 1970-1979',
                           'mean_yrbuilt_1980.1989' = 'Built 1980-1989',
                           'mean_yrbuilt_1990.1999' = 'Built 1990-1999',
                           'mean_yrbuilt_2000.2009' = 'Built 2000-2009',
                           'mean_yrbuilt_2010.2013' = 'Built 2010-2013',
                           'mean_yrbuilt_2014plus' = 'Built After 2014')) %>%
  spread(variable, value) %>%
  write_csv('~/git/dspg20broadbandERS/output/Public-Data/fitness-for-use-county-level-va.csv')

# long county table for use later
va_table_long <- va_table_wide %>%
  gather('variable', 'value', 2:41)

# read in RUCA codes
RUCA <- read_csv('~/git/dspg20broadbandERS/data/census-geographies/RUCA.csv')
RUCA <- RUCA %>%
  dplyr::select(-FIPS, -State) %>%
  dplyr::select(-RUCA2) %>%
  rename(RUCA = RUCA1) %>%
  mutate(GEOID = as.double(GEOID))

# rewrite tract table
tract_table_wide <- read_csv('~/git/dspg20broadbandERS/data/acs-cl-joined/ffu_va.csv')
tract_table_wide <- tract_table_wide %>%
  gather('variable', 'value', 2:40) %>%
  mutate(variable = recode_factor(variable, 
                                  'ffu_housing_units_total' = 'Total Housing Units',
                                  'ffu_occupancy_status_occupied' = 'Total Occupied Units',
                                  'ffu_occupancy_status_vacant' = 'Total Vacant Units',
                                  'ffu_value_less10' = 'Value Below $10,000',
                                  'ffu_value_10.15' = 'Value $10,000-$15,000',
                                  'ffu_value_15.20' = 'Value $15,000-$20,000',
                                  'ffu_value_20.25' = 'Value $20,000-$25,000',
                                  'ffu_value_25.30' = 'Value $25,000-$30,000',
                                  'ffu_value_30.35' = 'Value $30,000-$35,000',
                                  'ffu_value_35.40' = 'Value $35,000-$40,000',
                                  'ffu_value_40.50' = 'Value $40,000-$50,000',
                                  'ffu_value_50.60' = 'Value $50,000-$60,000',
                                  'ffu_value_60.70' = 'Value $60,000-$70,000',
                                  'ffu_value_70.80' = 'Value $70,000-$80,000',
                                  'ffu_value_80.90' = 'Value $80,000-$90,000',
                                  'ffu_value_90.100' = 'Value $90,000-$100,000',
                                  'ffu_value_100.125' = 'Value $100,000-$125,000',
                                  'ffu_value_125.150' = 'Value $125,000-$150,000',
                                  'ffu_value_150.175' = 'Value $150,000-175,000',
                                  'ffu_value_175.200' = 'Value $175,000-$200,000',
                                  'ffu_value_200.250' = 'Value $200,000-$250,000',
                                  'ffu_value_250.300' = 'Value $250,000-$300,000',
                                  'ffu_value_300.400' = 'Value $300,000-$400,000',
                                  'ffu_value_400.500' = 'Value $400,000-$500,000',
                                  'ffu_value_500.750' = 'Value $500,000-$750,000',
                                  'ffu_value_750.1000' = 'Value $750,000-$1,000,000',
                                  'ffu_value_1000.1500' = 'Value $1,000,000-$1,500,000',
                                  'ffu_value_1500.2000' = 'Value $1,500,000-$2,000,000',
                                  'ffu_value_2000plus' = 'Value Over $2,000,000',
                                  'ffu_yrbuilt_1939less' = 'Built Before 1939',
                                  'ffu_yrbuilt_1940.1949' = 'Built 1940-1949',
                                  'ffu_yrbuilt_1950.1959' = 'Built 1950-1959',
                                  'ffu_yrbuilt_1960.1969' = 'Built 1960-1969',
                                  'ffu_yrbuilt_1970.1979' = 'Built 1970-1979',
                                  'ffu_yrbuilt_1980.1989' = 'Built 1980-1989',
                                  'ffu_yrbuilt_1990.1999' = 'Built 1990-1999',
                                  'ffu_yrbuilt_2000.2009' = 'Built 2000-2009',
                                  'ffu_yrbuilt_2010.2013' = 'Built 2010-2013',
                                  'ffu_yrbuilt_2014plus' = 'Built After 2014')) %>%
  spread(variable, value) %>%
  left_join(RUCA, by = 'GEOID') %>%
  write_csv('~/git/dspg20broadbandERS/output/Public-Data/fitness-for-use-tract-level-va.csv')


# label data
vacounties_df <- va_table_wide %>%
  mutate(pctrural_label = paste0(round(percent_rural*100, 1), '% rural'),
         nodata_label = ifelse(mean_ffu_housing_units_total == 'NaN', ', No Data Available', ""),
         full_label = paste0(County, ', ', pctrural_label, nodata_label)) %>%
  dplyr::select(full_label)
vacounties_labels <- unique(vacounties_df$full_label)
write_rds(vacounties_labels, "~/git/dspg20broadbandERS/dashboard/data/vacounties_labels.RDS")
vacounties <- unique(va_table_long$County)
write_rds(vacounties, "~/git/dspg20broadbandERS/dashboard/data/vacounties.RDS")

# map data
ffu_va <- read_csv('~/git/dspg20broadbandERS/dashboard/data/ffu_va_ruca.csv')
va_tracts <- tracts(state = '51', year = 2018)
ffu_merged <- geo_join(va_tracts, ffu_va, "GEOID", "GEOID")

write_rds(ffu_merged, "~/git/dspg20broadbandERS/dashboard/data/ffu_merged.RDS")

# variable subsets of county data
write_rds(va_table_long, "~/git/dspg20broadbandERS/dashboard/data/va_table_long.RDS")

ffu_va_value <- va_table_long %>% filter(str_detect(variable, "Value"))
ffu_va_yrbuilt <- va_table_long %>% filter(str_detect(variable, "Built"))
ffu_va_housing <- va_table_long %>% filter(str_detect(variable, "Units"))

write_rds(ffu_va_value, "~/git/dspg20broadbandERS/dashboard/data/ffu_va_value.RDS")
write_rds(ffu_va_yrbuilt, "~/git/dspg20broadbandERS/dashboard/data/ffu_va_yrbuilt.RDS")
write_rds(ffu_va_housing, "~/git/dspg20broadbandERS/dashboard/data/ffu_va_housing.RDS")
