
# Sets Working Directory --------------------------------------------------
setwd("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/code/hampton_roads_home_ownership")


#saves shapefiles for future use
tigris_use_cache = TRUE

#identifies counties by fip code
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)


# Pulls ACS Housing Data for Each County of Hampton Roads and ggplots them----------------------------------

#creates table with all variables
housing_tbl <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c("S2502_C02_003", "S2502_C04_003", "S2502_C06_003"),
                         geometry = TRUE)

#pulls and maps overall population of homeowners in proportion to total home occupancy
tot_hm_own <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      variables = c(overall_owned_housing = "S2502_C03_001"),
                      summary_var = "S2502_C01_001",
                      geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = estimate)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "Homeowners Proportionate to Total Population Occupancy",
       x = "Longitude",
       y = "Latitude",
       caption = "ACS 2019 5 Year Estimates Table S2502")

tot_hm_own

#pulls and maps overall population of renters in proportion to total home occupancy
tot_hm_rent <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c(overall_rented_housing = "S2502_C05_001"),
                       summary_var = "S2502_C01_001",
                       geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>%
  mutate(Percent = round(Percent, 2))
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = estimate)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "Home Renters Proportionate to Total Population Occupancy",
       x = "Longitude",
       y = "Latitude",
       caption = "ACS 2019 5 Year Estimates Table S2502")

tot_hm_rent

#pulls 2019 ACS and maps black homeowners in proportion to total black home occupancy
b_hm_own <- get_acs(geography = "county",
                  state = "VA",
                  county = county_fips,
                  variables = c(black_owned_housing = "S2502_C03_003"),
                  summary_var = "S2502_C01_003",
                  geometry = TRUE) %>% 
                  mutate(Percent = 100 * (estimate / summary_est)) %>% 
                  mutate(Percent = round(Percent, 2)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = Percent)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "",
       x = "",
       y = "",
       caption = "ACS 2019 5 Year Estimates Table S2502")

b_hm_own

#pulls 2019 ACS and maps black renters in proportion to total black home occupancy
b_hm_rent <- get_acs(geography = "county",
                     state = "VA",
                     county = county_fips,
                     variables = c(black_rented_housing = "S2502_C05_003"),
                     summary_var = "S2502_C01_003",
                     geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = estimate)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"),size = 2) +
  labs(title = "Black Renters Proportionate to Total Black Occupancy",
       x = "",
       y = "",
       caption = "ACS 2019 5 Year Estimates Table S2502")


# Pulls ACS Black Home Owner Data-----------------------------------

#spalette for leaflet maps
pal <- colorNumeric(palette = "viridis", domain = b_hm_own$Percent, reverse = TRUE)

#pulls 2019 black home owner data
b_hm_19 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_fips,
                    survey = "acs5",
                    variables = c(black_owned_housing = "S2502_C03_003"),
                    summary_var = "S2502_C01_003",
                    geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2018 black home owner data
b_hm_18 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c(black_owned_housing = "S2502_C03_003"),
                       summary_var = "S2502_C01_003",
                       year = 2018,
                       geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2017 black home owner data
b_hm_17 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c(black_owned_housing = "S2502_C03_003"),
                       summary_var = "S2502_C01_003",
                       year = 2017,
                       geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2016 black home owner data
b_hm_16 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c("S2502_C02_003", "S2502_C02_001","S2502_C01_003", "S2502_C01_001"),
                       output = "wide",
                       year = 2016,
                       geometry = TRUE) %>% 
  mutate(S2502_C02_003E = (S2502_C02_003E / 100)) %>% 
  mutate(Number = (S2502_C02_003E * S2502_C02_001E)) %>% 
  mutate(S2502_C01_003E = (S2502_C01_003E / 100)) %>% 
  mutate(SumNumber = (S2502_C01_003E * S2502_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2015 black home owner data
b_hm_15 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c("S2502_C02_003", "S2502_C02_001","S2502_C01_003", "S2502_C01_001"),
                       output = "wide",
                       year = 2015,
                       geometry = TRUE) %>% 
  mutate(S2502_C02_003E = (S2502_C02_003E / 100)) %>% 
  mutate(Number = (S2502_C02_003E * S2502_C02_001E)) %>% 
  mutate(S2502_C01_003E = (S2502_C01_003E / 100)) %>% 
  mutate(SumNumber = (S2502_C01_003E * S2502_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2014 black home owner data
b_hm_14 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c("S2502_C02_003", "S2502_C02_001","S2502_C01_003", "S2502_C01_001"),
                       output = "wide",
                       year = 2014,
                       geometry = TRUE) %>% 
  mutate(S2502_C02_003E = (S2502_C02_003E / 100)) %>% 
  mutate(Number = (S2502_C02_003E * S2502_C02_001E)) %>% 
  mutate(S2502_C01_003E = (S2502_C01_003E / 100)) %>% 
  mutate(SumNumber = (S2502_C01_003E * S2502_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2013 black home owner data
b_hm_13 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c("S2502_C02_003", "S2502_C02_001","S2502_C01_003", "S2502_C01_001"),
                       output = "wide",
                       year = 2013,
                       geometry = TRUE) %>% 
  mutate(S2502_C02_003E = (S2502_C02_003E / 100)) %>% 
  mutate(Number = (S2502_C02_003E * S2502_C02_001E)) %>% 
  mutate(S2502_C01_003E = (S2502_C01_003E / 100)) %>% 
  mutate(SumNumber = (S2502_C01_003E * S2502_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2012 black home owner data
b_hm_12 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c("S2502_C02_003", "S2502_C02_001","S2502_C01_003", "S2502_C01_001"),
                       output = "wide",
                       year = 2012,
                       geometry = TRUE) %>% 
  mutate(S2502_C02_003E = (S2502_C02_003E / 100)) %>% 
  mutate(Number = (S2502_C02_003E * S2502_C02_001E)) %>% 
  mutate(S2502_C01_003E = (S2502_C01_003E / 100)) %>% 
  mutate(SumNumber = (S2502_C01_003E * S2502_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2011 black home owner data
b_hm_11 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c("S2502_C02_003", "S2502_C02_001","S2502_C01_003", "S2502_C01_001"),
                       output = "wide",
                       year = 2011,
                       geometry = TRUE) %>% 
  mutate(S2502_C02_003E = (S2502_C02_003E / 100)) %>% 
  mutate(Number = (S2502_C02_003E * S2502_C02_001E)) %>% 
  mutate(S2502_C01_003E = (S2502_C01_003E / 100)) %>% 
  mutate(SumNumber = (S2502_C01_003E * S2502_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2010 black home owner data
b_hm_10 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c("S2502_C02_003", "S2502_C02_001","S2502_C01_003", "S2502_C01_001"),
                       output = "wide",
                       year = 2010,
                       geometry = TRUE) %>% 
  mutate(S2502_C02_003E = (S2502_C02_003E / 100)) %>% 
  mutate(Number = (S2502_C02_003E * S2502_C02_001E)) %>% 
  mutate(S2502_C01_003E = (S2502_C01_003E / 100)) %>% 
  mutate(SumNumber = (S2502_C01_003E * S2502_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

# Pulls ACS Total Home Owner Data -------

#pulls 2019 total population home owner data

tot_hm_19 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      survey = "acs5",
                      year = 2019,
                      variables = c(overall_owned_housing = "S2502_C03_001"),
                      summary_var = "S2502_C01_001",
                      geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2018 total population home owner data

tot_hm_18 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      variables = c(overall_owned_housing = "S2502_C03_001"),
                      summary_var = "S2502_C01_001",
                      year = 2018,
                      geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2017 total population home owner data

tot_hm_17 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c(overall_owned_housing = "S2502_C02_001"),
                         summary_var = "S2502_C01_001",
                         year = 2017,
                         geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2016 total population home owner data
tot_hm_16 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c(overall_owned_housing = "S2502_C02_001"),
                         summary_var = "S2502_C01_001",
                         year = 2016,
                         geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2015 total population home owner data
tot_hm_15 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c(overall_owned_housing = "S2502_C02_001"),
                         summary_var = "S2502_C01_001",
                         year = 2015,
                         geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2014 total population home owner data
tot_hm_14 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c(overall_owned_housing = "S2502_C02_001"),
                         summary_var = "S2502_C01_001",
                         year = 2014,
                         geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2013 total population home owner data
tot_hm_13 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c(overall_owned_housing = "S2502_C02_001"),
                         summary_var = "S2502_C01_001",
                         year = 2013,
                         geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2012 total population home owner data
tot_hm_12 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c(overall_owned_housing = "S2502_C02_001"),
                         summary_var = "S2502_C01_001",
                         year = 2012,
                         geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2011 total population home owner data
tot_hm_11<- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c(overall_owned_housing = "S2502_C02_001"),
                         summary_var = "S2502_C01_001",
                         year = 2011,
                         geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

#pulls 2010 total population home owner data
tot_hm_10 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c(overall_owned_housing = "S2502_C02_001"),
                         summary_var = "S2502_C01_001",
                         year = 2010,
                         geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))
  



# RDS Conversions ---------------------------------------------------------

##Black
#2019 Black
write_rds(b_hm_19, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2019.rds")
b_hm_19 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2019.rds")
b_hm_19 <- st_transform(b_hm_19)

#2018 Black
write_rds(b_hm_18, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2018.rds")
b_hm_18 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2018.rds")
b_hm_18 <- st_transform(b_hm_18)

#2017 Black
write_rds(b_hm_17, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2017.rds")
b_hm_17 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2017.rds")
b_hm_17 <- st_transform(b_hm_17)

#2016 Black
write_rds(b_hm_16, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2016.rds")
b_hm_16 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2016.rds")
b_hm_16 <- st_transform(b_hm_16)

#2015 Black
write_rds(b_hm_15, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2015.rds")
b_hm_15 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2015.rds")
b_hm_15 <- st_transform(b_hm_15)

#2014 Black
write_rds(b_hm_14, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2014.rds")
b_hm_14 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2014.rds")
b_hm_14 <- st_transform(b_hm_14)

#2013 Black
write_rds(b_hm_13, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2013.rds")
b_hm_13 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2013.rds")
b_hm_13 <- st_transform(b_hm_13)

#2012 Black
write_rds(b_hm_12, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2012.rds")
b_hm_12 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2012.rds")
b_hm_12 <- st_transform(b_hm_12)

#2011 Black
write_rds(b_hm_11, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2011.rds")
b_hm_11 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2011.rds")
b_hm_11 <- st_transform(b_hm_11)

#2010 Black
write_rds(b_hm_10, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2010.rds")
b_hm_10 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/bhmown2010.rds")
b_hm_10 <- st_transform(b_hm_10)

##Total
#2019 Total
write_rds(tot_hm_19, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2019.rds")
tot_hm_19 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2019.rds")
tot_hm_19 <- st_transform(tot_hm_19)

#2018 Total
write_rds(tot_hm_18, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2018.rds")
tot_hm_18 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2018.rds")
tot_hm_18 <- st_transform(tot_hm_18)

#2017 Total
write_rds(tot_hm_17, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2017.rds")
tot_hm_17 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2017.rds")
tot_hm_17 <- st_transform(tot_hm_17)

#2016 Total
write_rds(tot_hm_16, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2016.rds")
tot_hm_16 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2016.rds")
tot_hm_16 <- st_transform(tot_hm_16)

#2015 Total
write_rds(tot_hm_15, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2015.rds")
tot_hm_15 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2015.rds")
tot_hm_15 <- st_transform(tot_hm_15)

#2014 Total
write_rds(tot_hm_14, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2014.rds")
tot_hm_14 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2014.rds")
tot_hm_14 <- st_transform(tot_hm_14)

#2013 Total
write_rds(tot_hm_13, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2013.rds")
tot_hm_13 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2013.rds")
tot_hm_13 <- st_transform(tot_hm_13)

#2012 Total
write_rds(tot_hm_12, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2012.rds")
tot_hm_12 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2012.rds")
tot_hm_12 <- st_transform(tot_hm_12)

#2011 Total
write_rds(tot_hm_11, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2011.rds")
tot_hm_11 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2011.rds")
tot_hm_11 <- st_transform(tot_hm_11)

#2010 Total
write_rds(tot_hm_10, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2010.rds")
tot_hm_10 <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2502FiveYearEstimates/tothmown2010.rds")
tot_hm_10 <- st_transform(tot_hm_10)

# #negative data charts
# pick_n <- function(id){​​​​​​​
#   dataFiltered <- filter(neg_data, ID== id)
#   
#   p <- ggplot(dataFiltered, aes(x=Year, y=estimate)) + 
#     geom_line(position = "identity", aes(group=1)) +
#     ggtitle("AADT from 2010 to 2020") +
#     theme(plot.title = element_text(hjust=0.5),
#           axis.title.x = element_blank()) +
#     labs(y ="Annual Average Daily Traffic") +
#     theme_minimal()
#   p
#   return(p)
# }​​​​​​​
# 
# 
# #loops through all the data (3 routes)
# r <- lapply(1:length(unique(neg_data$ID)), function(i) {​​​​​​​
#   pick_n(neg_data$ID[i])
# }​​​​​​​)
# 
# 
# 
# 
# ​[7/23 8:57 PM] Prisbe, Christina
# then use the argument:  popup = popup graph(r)    in your addMarkers or addCircleMarkers


# Line Plot ---------------------------------------------------------------

#adds year element to each data frame for total population
tot_hm_10 <- tot_hm_10 %>% 
  mutate(Year = "2010")

tot_hm_11 <- tot_hm_11 %>% 
  mutate(Year = "2011")

tot_hm_12 <- tot_hm_12 %>% 
  mutate(Year = "2012")

tot_hm_13 <- tot_hm_13 %>% 
  mutate(Year = "2013")

tot_hm_14 <- tot_hm_14 %>% 
  mutate(Year = "2014")

tot_hm_15 <- tot_hm_15 %>% 
  mutate(Year = "2015")

tot_hm_16 <- tot_hm_16 %>% 
  mutate(Year = "2016")

tot_hm_17 <- tot_hm_17 %>% 
  mutate(Year = "2017")

tot_hm_18 <- tot_hm_18 %>% 
  mutate(Year = "2018")

tot_hm_19 <- tot_hm_19 %>% 
  mutate(Year = "2019")

#adds year element to each data frame for black population
b_hm_10 <- b_hm_10 %>% 
  mutate(Year = "2010") %>% 
  mutate(variable = "black_owned_housing") %>% 
  select(NAME, variable, Percent, Year)

b_hm_11 <- b_hm_11 %>% 
  mutate(Year = "2011") %>% 
  mutate(variable = "black_owned_housing") %>% 
  select(NAME, variable, Percent, Year)


b_hm_12 <- b_hm_12 %>% 
  mutate(Year = "2012") %>% 
  mutate(variable = "black_owned_housing") %>% 
  select(NAME, variable, Percent, Year)

b_hm_13 <- b_hm_13 %>% 
  mutate(Year = "2013") %>% 
  mutate(variable = "black_owned_housing") %>% 
  select(NAME, variable, Percent, Year)

b_hm_14 <- b_hm_14 %>% 
  mutate(Year = "2014") %>% 
  mutate(variable = "black_owned_housing") %>% 
  select(NAME, variable, Percent, Year)

b_hm_15 <- b_hm_15 %>% 
  mutate(Year = "2015") %>% 
  mutate(variable = "black_owned_housing") %>% 
  select(NAME, variable, Percent, Year)

b_hm_16 <- b_hm_16 %>% 
  mutate(Year = "2016") %>% 
  mutate(variable = "black_owned_housing") %>% 
  select(NAME, variable, Percent, Year)

b_hm_17 <- b_hm_17 %>% 
  mutate(Year = "2017") %>% 
  select(NAME, variable, Percent, Year)

b_hm_18 <- b_hm_18 %>% 
  mutate(Year = "2018") %>% 
  select(NAME, variable, Percent, Year)

b_hm_19 <- b_hm_19 %>% 
  mutate(Year = "2019") %>% 
  select(NAME, variable, Percent, Year)

#combines data frames
tot_hm <- rbind(tot_hm_10, tot_hm_11, tot_hm_12, tot_hm_13, tot_hm_14, tot_hm_15, tot_hm_16, tot_hm_17, tot_hm_18, tot_hm_19)
tot_hm <- tot_hm %>% 
  select(NAME, variable, Percent, Year)

b_hm <- rbind(b_hm_10, b_hm_11, b_hm_12, b_hm_13, b_hm_14, b_hm_15, b_hm_16, b_hm_17, b_hm_18, b_hm_19)

all_hm_data <- rbind(tot_hm, b_hm)

line <- all_hm_data %>% 
  filter(NAME == "Gloucester County, Virginia") %>% 
  ggplot(aes(x = Year, y = Percent)) +
  geom_line(aes(group = black_owned_housing, color = "blue")) +
  geom_line(aes(group = overall_owned_housing, color = "red"))

glouc_data <- all_hm_data %>% 
  filter(NAME == "Gloucester County, Virginia")

glouc_line <- glouc_data %>% 
  ggplot(aes(x = Year, )) +
  geom_line()
  

  




