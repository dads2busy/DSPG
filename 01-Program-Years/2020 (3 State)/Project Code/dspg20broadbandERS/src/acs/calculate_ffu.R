#acs - external / moe
calc_ffu<- function(acs_cl_joined_data){
  ffu<- acs_cl_joined_data %>%
    mutate(ffu_housing_units_total = (housing_units_total - cl_occstatus_total)/housing_units_total_moe,
           ffu_value_less10 = (value_less10 - cl_value_less10)/value_less10_moe,
           ffu_value_10.15 = (value_10.15 - cl_value_10.15)/value_10.15_moe,
           ffu_value_15.20 = (value_15.20 - cl_value_15.20)/value_15.20_moe,
           ffu_value_20.25 = (value_20.25 - cl_value_20.25)/value_20.25_moe,
           ffu_value_25.30 = (value_25.30 - cl_value_25.30)/value_25.30_moe,
           ffu_value_30.35 = (value_30.35 - cl_value_30.35)/value_30.35_moe,
           ffu_value_35.40 = (value_35.40 - cl_value_35.40)/value_35.40_moe,
           ffu_value_40.50 = (value_40.50 - cl_value_40.50)/value_40.50_moe,
           ffu_value_50.60 = (value_50.60 - cl_value_50.60)/value_50.60_moe,
           ffu_value_60.70 = (value_60.70 - cl_value_60.70)/value_60.70_moe,
           ffu_value_70.80 = (value_70.80 - cl_value_70.80)/value_70.80_moe,
           ffu_value_80.90 = (value_80.90 - cl_value_80.90)/value_80.90_moe,
           ffu_value_90.100 = (value_90.100 - cl_value_90.100)/value_90.100_moe,
           ffu_value_100.125 = (value_100.125 - cl_value_100.125)/value_100.125_moe,
           ffu_value_125.150 = (value_125.150 - cl_value_125.150)/value_125.150_moe,
           ffu_value_150.175 = (value_150.175 - cl_value_150.175)/value_150.175_moe,
           ffu_value_175.200 = (value_175.200 - cl_value_175.200)/value_175.200_moe,
           ffu_value_200.250 = (value_200.250 - cl_value_200.250)/value_200.250_moe,
           ffu_value_250.300 = (value_250.300 - cl_value_250.300)/value_250.300_moe,
           ffu_value_300.400 = (value_300.400 - cl_value_300.400)/value_300.400_moe,
           ffu_value_400.500 = (value_400.500 - cl_value_400.500)/value_400.500_moe,
           ffu_value_500.750 = (value_500.750 - cl_value_500.750)/value_500.750_moe,
           ffu_value_750.1000 = (value_750.1000 - cl_value_750.1000)/value_750.1000_moe,
           ffu_value_1000.1500 = (value_1000.1500 - cl_value_1000.1500)/value_1000.1500_moe,
           ffu_value_1500.2000 = (value_1500.2000 - cl_value_1500.2000)/value_1500.2000_moe,
           ffu_value_2000plus = (value_2000plus - cl_value_2000plus)/value_2000plus_moe,
           ffu_yrbuilt_1939less = (yrbuilt_earlier.1939 - cl_yrbuilt_1939less)/yrbuilt_earlier.1939_moe,
           ffu_yrbuilt_1940.1949 = (yrbuilt_1940.1949 - cl_yrbuilt_1940.1949)/yrbuilt_1940.1949_moe,
           ffu_yrbuilt_1950.1959 = (yrbuilt_1950.1959 - cl_yrbuilt_1950.1959)/yrbuilt_1950.1959_moe,
           ffu_yrbuilt_1960.1969 = (yrbuilt_1960.1969 - cl_yrbuilt_1960.1969)/yrbuilt_1960.1969_moe,
           ffu_yrbuilt_1970.1979 = (yrbuilt_1970.1979 - cl_yrbuilt_1970.1979)/yrbuilt_1970.1979_moe,
           ffu_yrbuilt_1980.1989 = (yrbuilt_1980.1989 - cl_yrbuilt_1980.1989)/yrbuilt_1980.1989_moe,
           ffu_yrbuilt_1990.1999 = (yrbuilt_1990.1999 - cl_yrbuilt_1990.1999)/yrbuilt_1990.1999_moe,
           ffu_yrbuilt_2000.2009 = (yrbuilt_2000.2009 - cl_yrbuilt_2000.2009)/yrbuilt_2000.2009_moe,
           ffu_yrbuilt_2010.2013 = (yrbuilt_2010.2013 - cl_yrbuilt_2010.2013)/yrbuilt_2010.2013_moe,
           ffu_yrbuilt_2014plus = (yrbuilt_2014.plus - cl_yrbuilt_2014plus)/yrbuilt_2014.plus_moe,
           ffu_occupancy_status_occupied = (occupancy_status_occupied - cl_occupied)/occupancy_status_occupied_moe,
           ffu_occupancy_status_vacant = (occupancy_status_vacant - cl_vacant)/occupancy_status_vacant_moe)
}
ffu_va <- calc_ffu(acs_cl_joined)

ffu_va <- select(ffu_va, c(GEOID, starts_with('ffu')))

write_csv(ffu_va, '~/git/dspg20broadbandERS/data/acs-cl-joined/ffu_va.csv')
