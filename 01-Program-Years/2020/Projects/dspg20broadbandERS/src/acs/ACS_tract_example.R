# function to call ACS demographic variables by Census Tract for a given year
library(tidycensus)
library(maps)
library(data.table)

# get your API key at https://api.census.gov/data/key_signup.html
#census_api_key("YOUR_KEY")

# continental + AK, HI state fips
state_fips <- c( unique(state.fips$fips), 2, 15 )
state_fips <- formatC(state_fips,width=2,flag=0)

# look up table names and variables at https://data.census.gov/cedsci/
acs_vars <- c("B15002_001","B15002_003","B15002_004","B15002_005","B15002_006","B15002_007","B15002_008","B15002_009","B15002_010","B15002_011",
              "B15002_020","B15002_021","B15002_022","B15002_023","B15002_024","B15002_025","B15002_026","B15002_027","B15002_028",
              "B17001_001","B17001_002",
              "B01001_001","B01001_020","B01001_021","B01001_022","B01001_023","B01001_024","B01001_025",
              "B01001_044","B01001_045","B01001_046","B01001_047","B01001_048","B01001_049",
              "B03003_001","B03003_003",
              "B02001_001","B02001_003",
              "B09019_002","B09019_003",
              "B05002_001","B05002_013",
              "B19013_001",
              "B25077_001",
              "B23025_003","B23025_005",
              "B25001_001", "B25002_001", "B25002_002", "B25002_003", "B25003_001", "B25003_002", "B25003_003", 
              "B25024_001", "B25024_002", "B25024_003", "B25024_004", "B25024_005", "B25024_006", "B25024_007", "B25024_008", "B25024_009", "B25024_010", "B25024_011",
              "B25035_001", "B25041_001", "B25041_002", "B25041_003", "B25041_004", "B25041_005", "B25041_006", "B25041_007", "B25064_001", "B25065_001")

# get ACS variables for a given year for all US census tracts
# for 2006-2010 5-year don't include family, employment (no tables in API)
get_acs_vars <- function(year, vars){
  tmp <- list()
  for(i in 1:length(state_fips)){
    tmp[[i]] <- get_acs(geography="tract",state=state_fips[i],variables=vars,year=year,cache_table=TRUE,output="wide")
  }
  acs_est <- rbindlist(tmp)
  
  # compute estimates from ACS variables
  acs_estimates <- acs_est %>% transmute(
    GEOID=GEOID,
    population = B01001_001E,
    hs_or_less = (B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E+B15002_009E+B15002_010E+B15002_011E+
                    B15002_020E+B15002_021E+B15002_022E+B15002_023E+B15002_024E+B15002_025E+B15002_026E+B15002_027E+B15002_028E) / B15002_001E,
    poverty = B17001_002E / B17001_001E,
    age_65_older = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+
                      B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E)/ B01001_001E,
    hispanic = B03003_003E / B03003_001E,
    black = B02001_003E / B02001_001E,
    foreign = B05002_013E / B05002_001E,
    median_income = B19013_001E,
    median_value = B25077_001E,
    median_yrbuilt = B25035_001E,
    bed_total = B25041_001E,
    bed_none = B25041_002E,
    bed_1 = B25041_003E,
    bed_2 = B25041_004E,
    bed_3 = B25041_005E,
    bed_4 = B25041_006E,
    bed_5plus= B25041_007E,
    rent_mediangross = B25064_001E, 
    rent_aggreggross = B25065_001E,
    hunits_total = B25001_001E,
    occstatus_total = B25002_001E,
    occstatus_occup = B25002_002E,
    occstatus_vac = B25002_003E,
    tenure_total= B25003_001E,
    tenure_own = B25003_002E,
    tenure_rent = B25003_003E,
    unitno_total = B25024_001E,
    unitno_1det = B25024_002E,
    unitno_1at = B25024_003E,
    unitno_2 = B25024_004E,
    unitno_3or4 = B25024_005E,
    unitno_5to9 = B25024_006E,
    unitno_10to19 = B25024_007E,
    unitno_20to49 = B25024_008E,
    unitno_50plus = B25024_009E,
    unitno_mobile = B25024_010E,
    unitno_other = B25024_011E
  )
  if(year >= 2012) { acs_estimates <- cbind(acs_estimates,
                                            acs_est %>% transmute(  
                                              family = B09019_003E / B09019_002E,
                                              unemployment = B23025_005E/B23025_003E
                                            )
  )}
  names(acs_estimates)[2:length(names(acs_estimates))] <-
    paste0(names(acs_estimates)[2:length(names(acs_estimates))],"_",year)
  return(acs_estimates)  
}

# example: create estimates by tract for 2018
acs_estimates_2018 <- get_acs_vars(2018, acs_vars)




