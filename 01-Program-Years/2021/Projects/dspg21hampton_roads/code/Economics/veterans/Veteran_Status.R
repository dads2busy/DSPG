# Installs and Loads  Required Packages-----------------------------------------------------
install.packages("tidycensus", "tidyverse", "dplyr", "tigris", "ggplot2", "sf")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(fontawesome)
library(rgdal)


#saves shapefiles for future use
tigris_use_cache = TRUE

#identifies counties by fip code
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)


# Pulls Virginia Table ----------------------------------------------------
va_gen_vet_status <- get_acs(geography = "state",
                             state = "VA",
                             variables = c(va_vet_status = "S2101_C03_001"),
                             summary_var = "S2101_C01_001",
                             geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est))

va_gen_vet_status


gen_vet_status_plot <- get_acs(geography = "county",
                          state = "VA",
                          county = county_fips,
                          variables = c(gen_vet_proportion = "S2101_C03_001"),
                          summary_var = "S2101_C01_001",
                          geometry = TRUE) %>% 
  mutate(estimate = 100 * (sum(estimate) / sum(summary_est)))
  

gen_vet_status_plot

black_vet_status_plot <- get_acs(geography = "county",
                               state = "VA",
                               county = county_fips,
                               variables = c(black_vet_proportion = "S2101_C03_015"),
                               summary_var = "S2101_C01_015",
                               geometry = TRUE) %>% 
  mutate(estimate = 100 * (sum(estimate) / sum(summary_est)))

black_vet_status_plot

# Pulls and maps Static Veteran Status Tables ------------------------------------
##general veteran status static map
gen_vet_status <- get_acs(geography = "county",
                          state = "VA",
                          county = county_fips,
                          variables = c(gen_vet_proportion = "S2101_C03_001"),
                          summary_var = "S2101_C01_001",
                          geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = Percent)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "",
       x = "",
       y = "",
       caption = "ACS 2019 5 Year Estimates Table S2101")

gen_vet_status

##black veteran status static map
black_vet_status <- get_acs(geography = "county",
                            state = "VA",
                            county = county_fips,
                            variables = c(black_vet_proportion = "S2101_C03_015"),
                            summary_var = "S2101_C01_015",
                            geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = Percent)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "",
       x = "",
       y = "",
       caption = "ACS 2019 5 Year Estimates Table S2101")

black_vet_status

# Pulls ACS Black Veteran Status Data -------------------------------------------------------

##Pulls 2019 ACS Data and Geometry and writes rds file
  b_vet_19 <- get_acs(geography = "county",
                              state = "VA",
                              county = county_fips,
                              variables = c("Black Veterans" = "S2101_C03_015"),
                              summary_var = "S2101_C01_015",
                              survey = "acs5",
                              year = 2019,
                              geometry = TRUE) %>% 
    mutate(Percent = 100 * (estimate / summary_est)) %>% 
    mutate(Percent = round(Percent, 2))
 
write_rds(b_vet_19, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2019.rds")
b_vet_19 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2019.rds")
b_vet_19 <- st_transform(b_vet_19)

##pulls 2018 ACS Data and Geometry and writes rds file
  b_vet_18 <- get_acs(geography = "county",
                                 state = "VA",
                                 county = county_fips,
                                 variables = c("Black Veterans" = "S2101_C03_015"),
                                 summary_var = "S2101_C01_015",
                                 survey = "acs5",
                                 year = 2018,
                                 geometry = TRUE) %>% 
    mutate(Percent = 100 * (estimate / summary_est)) %>% 
    mutate(Percent = round(Percent, 2))
  
write_rds(b_vet_18, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2018.rds")
b_vet_18 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2018.rds")
b_vet_18 <- st_transform(b_vet_18)
  
##pulls 2017 ACS Data and Geometry and writes rds file
  b_vet_17 <- get_acs(geography = "county",
                                 state = "VA",
                                 county = county_fips,
                                 variables = c("Black Veterans" = "S2101_C03_015"),
                                 summary_var = "S2101_C01_015",
                                 survey = "acs5",
                                 year = 2017,
                                 geometry = TRUE) %>% 
    mutate(Percent = 100 * (estimate / summary_est)) %>% 
    mutate(Percent = round(Percent, 2))
  
write_rds(b_vet_17, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2017.rds")
b_vet_17 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2017.rds")
b_vet_17 <- st_transform(b_vet_17)
  
##pulls 2016 ACS Data and Geometry and writes rds file
  b_vet_16 <- get_acs(geography = "county",
                                 state = "VA",
                                 county = county_fips,
                                 variables = c("Black Veterans" = "S2101_C03_015"),
                                 summary_var = "S2101_C01_015",
                                 survey = "acs5",
                                 year = 2016,
                                 geometry = TRUE) %>% 
    mutate(Percent = 100 * (estimate / summary_est)) %>% 
    mutate(Percent = round(Percent, 2))

write_rds(b_vet_16, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2016.rds")
b_vet_16 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2016.rds")
b_vet_16 <- st_transform(b_vet_16)

##pulls 2015 ACS Data and Geometry and writes rds file
  b_vet_15 <- get_acs(geography = "county",
                                 state = "VA",
                                 county = county_fips,
                                 variables = c("Black Veterans" = "S2101_C03_015"),
                                 summary_var = "S2101_C01_015",
                                 survey = "acs5",
                                 year = 2015,
                                 geometry = TRUE) %>% 
    mutate(Percent = 100 * (estimate / summary_est)) %>% 
    mutate(Percent = round(Percent, 2))
  
write_rds(b_vet_15, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2015.rds")
b_vet_15 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2015.rds")
b_vet_15 <- st_transform(b_vet_15)

#pulls 2014 ACS Data and Geometry and writes rds file
b_vet_14 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c("S2101_C02_016", "S2101_C02_001", "S2101_C01_016", "S2101_C01_001"),
                       output = "wide",
                       year = 2014,
                       geometry = TRUE) %>% 
  mutate(S2101_C02_016E = (S2101_C02_016E / 100)) %>% 
  mutate(Number = (S2101_C02_016E * S2101_C02_001E)) %>% 
  mutate(S2101_C01_016E = (S2101_C01_016E / 100)) %>% 
  mutate(SumNumber = (S2101_C01_016E * S2101_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

write_rds(b_vet_14, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2014.rds")
b_vet_14 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2014.rds")
b_vet_14 <- st_transform(b_vet_14)

#pulls 2013 ACS Data and Geometry and writes rds file
b_vet_13 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_fips,
                    variables = c("S2101_C02_016", "S2101_C02_001", "S2101_C01_016", "S2101_C01_001"),
                    output = "wide",
                    year = 2013,
                    geometry = TRUE) %>% 
  mutate(S2101_C02_016E = (S2101_C02_016E / 100)) %>% 
  mutate(Number = (S2101_C02_016E * S2101_C02_001E)) %>% 
  mutate(S2101_C01_016E = (S2101_C01_016E / 100)) %>% 
  mutate(SumNumber = (S2101_C01_016E * S2101_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

write_rds(b_vet_13, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2013.rds")
b_vet_13 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2013.rds")
b_vet_13 <- st_transform(b_vet_13)

#pulls 2012 ACS Data and Geometry and writes rds file
b_vet_12 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_fips,
                    variables = c("S2101_C02_016", "S2101_C02_001", "S2101_C01_016", "S2101_C01_001"),
                    output = "wide",
                    year = 2012,
                    geometry = TRUE) %>% 
  mutate(S2101_C02_016E = (S2101_C02_016E / 100)) %>% 
  mutate(Number = (S2101_C02_016E * S2101_C02_001E)) %>% 
  mutate(S2101_C01_016E = (S2101_C01_016E / 100)) %>% 
  mutate(SumNumber = (S2101_C01_016E * S2101_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

write_rds(b_vet_12, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2012.rds")
b_vet_12 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2012.rds")
b_vet_12 <- st_transform(b_vet_12)

#pulls 2011 ACS Data and Geometry and writes rds file
b_vet_11 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_fips,
                    variables = c("S2101_C02_016", "S2101_C02_001", "S2101_C01_016", "S2101_C01_001"),
                    output = "wide",
                    year = 2011,
                    geometry = TRUE) %>% 
  mutate(S2101_C02_016E = (S2101_C02_016E / 100)) %>% 
  mutate(Number = (S2101_C02_016E * S2101_C02_001E)) %>% 
  mutate(S2101_C01_016E = (S2101_C01_016E / 100)) %>% 
  mutate(SumNumber = (S2101_C01_016E * S2101_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

write_rds(b_vet_11, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2011.rds")
b_vet_11 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2011.rds")
b_vet_11 <- st_transform(b_vet_11)

#pulls 2010 ACS Data and Geometry and writes rds file
b_vet_10 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_fips,
                    variables = c("S2101_C02_016", "S2101_C02_001", "S2101_C01_016", "S2101_C01_001"),
                    output = "wide",
                    year = 2010,
                    geometry = TRUE) %>% 
  mutate(S2101_C02_016E = (S2101_C02_016E / 100)) %>% 
  mutate(Number = (S2101_C02_016E * S2101_C02_001E)) %>% 
  mutate(S2101_C01_016E = (S2101_C01_016E / 100)) %>% 
  mutate(SumNumber = (S2101_C01_016E * S2101_C01_001E)) %>% 
  mutate(Percent = 100 * (Number / SumNumber)) %>% 
  mutate(Percent = round(Percent, 2))

write_rds(b_vet_10, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2010.rds")
b_vet_10 <- readRDS("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/bveteran2010.rds")
b_vet_10 <- st_transform(b_vet_10)

###Creates data table containing military base names/branches and lat/lng
base_name <- c("Training Center Yorktown", "Naval Weapons Station Yorktown", "Fort Eustis", "Langley Air Force Base", "Fort Monroe", 
               "Naval Station Norfolk", "Naval Support Activity Norfolk", "Naval Amphibious Base Little Creek", "Joint Expiditionary Base", 
               "Naval Air Station Oceana", "Medical Center Portsmouth", "Norfolk Naval Shipyard", "Sector Hampton Roads", "Finance Center", "Naval Support Activity Northwest Annex")
branch <- c("Coast Guard", "Navy", "Army", "Air Force", "Army", "Navy", "Norfolk", "Navy", "Navy", "Navy", "Navy", "Navy", "Coast Guard", "Coast Guard", "Navy")
latitude <- c(37.239, 37.2136, 37.16049, 37.084616, 37.005051, 36.947398, 36.923381, 36.916897, 36.918057, 36.825638, 36.848233, 36.820086, 
              36.835, 36.8187, 36.643075)
longitude <- c(-76.5142, -76.4873, -76.58025, -76.360552, -76.309991, -76.31692, -76.303271, -76.190078, -76.009556, -76.033459, -76.306209, -76.300926, 
               -76.298, -76.2753, -76.278472)

military_bases <- data.frame(base_name, branch, latitude, longitude)

write_rds(military_bases, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/militarybases.rds")
military_bases <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2101FiveYearEstimates/militarybases.rds")
military_bases <- st_transform(military_bases)

pal <- colorNumeric(palette = "viridis", domain = b_vet_19$Percent, reverse = TRUE)


#####leaflet map
  b_vet_19 %>% 
  # st_transform() %>% 
  leaflet(options = leafletOptions(minZoom = 8)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), label = ~paste0(NAME,  " Black Veterans: ",
                                                                                                                   Percent, "%")) %>% 
  # addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch)) %>% 
  addLegend("topleft",
          pal = pal,
          values = ~ Percent,
          title = "Black Veterans",
          labFormat = labelFormat(suffix = "%"),
          opacity = 1)
  




  


  