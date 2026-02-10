# install.packages("tidycensus",dep=T)

tidycensus::census_api_key(key =  "13e823ad71a906bd1b67571bde3a9fd8e271fd63", overwrite = TRUE, install = T)

readRenviron("~/.Renviron")

# install.packacges ("tidyverse", dep=T)
# install.packages("dplyr")
# install.packages("leaflet")

setwd("C:/Users/chani/Google Drive/Virginia Tech/Teaching/DPSG/Hampton")

library(tidycensus)
library(tidyverse)
library(leaflet)


fips_codes <- c(51550,51620,51650,51700,51710,51735,51740,51800,51810,51830,
                51073,51093,51095,51115,51175,51199)

county_stats <- function(varcode) { 
  get_acs(geography = "county",
          state = 51,
          year = 2019,
          variables = varcode, 
          geometry = FALSE, cache_table=TRUE) %>% 
    filter(GEOID %in% fips_codes) %>%
    select(NAME, GEOID, estimate)
}


table_stats <- function(varcode) {
  get_acs(geography = "county",
          # county = fips_codes,
          state = 51,
          year = 2019,
          table = varcode,
          geometry = TRUE, cache_table=TRUE) %>%
    filter(GEOID %in% fips_codes) %>%
    select(GEOID, NAME, estimate, geometry)
}

percapita <- table_stats("B19301B")%>% rename(percapitatot=estimate) #Per capita income in the past 12 months (in 2019 inflation-adjusted dollars)

#Variable of interest for the Black population at the county level
femaleheadhousehold <- county_stats("S0901_C04_007") %>%  rename(femaleheadhousehold=estimate)
foodstampsnap <- county_stats("S2201_C04_026") %>%  rename(foodstampsnap=estimate) #Percent households receiving food stamp/SNAP#
mobility <- county_stats("S0701_C03_016")%>% rename(mobility=estimate) #Percent moved from different county, same state
grandchildren <- county_stats("S1002_C02_004")%>% rename(grandparent=estimate) #Percent distribution of grandparents responsible for grandchildren
marital <- county_stats ("S1201_C02_019")%>% rename(married=estimate) #Percent of population 15 and over now married
transport <- county_stats ("S0802_C02_013") %>% rename(cartransport=estimate) #Percent of pop that uses car,truck, or van to work (drove alone)
publictransport<-county_stats ("S0802_C04_013") %>% rename(pubtransport=estimate) #Percent of pop that uses car,truck, or van to work (drove alone)
compinternt <- county_stats ("S2802_C03_006") %>% rename(compinternet=estimate) #Percent with a computer with broadband internet subscription
nocomp <-county_stats ("S2802_C07_006") %>% rename(nocomp=estimate) #Percent with no computer in household



all_ACS5 <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                   list(femaleheadhousehold, foodstampsnap,
                        mobility, grandchildren, marital, transport,
                        publictransport,nocomp,compinternt, percapita))


# Female Head of House ----------------------------------------------------
fml <- get_acs(geography = "county",
               state = "VA",
               county = fips_codes,
               variables = c("Female Head of Household" = "S0901_C04_007"),
               survey = "acs5",
               year = 2019,
               geometry = TRUE)

write_rds (fml, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/fml.rds")
fml <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/fml.rds")
fml <- st_transform(fml)




fml <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/fml.rds")
colnames(fml)[4] <- "Percent"
fempal <- colorNumeric(palette = "viridis", domain = fml$Percent, reverse = TRUE)

fml_map <- fml %>% 
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ fempal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
              label = ~paste0(NAME, "</br>", variable, ": ", Percent, "%")) %>% 
  addLegend("topleft",
            pal = fempal,
            values = ~ Percent,
            title = "Female HOH",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)



# Foodstamps --------------------------------------------------------------
foodstmp <- get_acs(geography = "county",
                    state = "VA",
                    county = fips_codes,
                    variables = c("Household Receives Foodstamps" = "S2201_C04_026"),
                    survey = "acs5",
                    year = 2019,
                    geometry = TRUE)

write_rds (foodstmp, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/foodstmp.rds")
foodstmp <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/foodstmp.rds")
foodstmp <- st_transform(foodstmp)

foodstmp <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/foodstmp.rds")
colnames(foodstmp)[4] <- "Percent"
foodpal <- colorNumeric(palette = "viridis", domain = foodstmp$Percent, reverse = TRUE)

foodstmp_map <- foodstmp %>% 
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ foodpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
              label = ~paste0(NAME, "</br>", variable, ": ", Percent, "%")) %>% 
  addLegend("topleft",
            pal = foodpal,
            values = ~ Percent,
            title = "Food Stamps",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)


# Mobility ----------------------------------------------------------------
mobile <- get_acs(geography = "county",
                  state = "VA",
                  county = fips_codes,
                  variables = c("County Movement" = "S0701_C03_016"),
                  survey = "acs5",
                  year = 2019,
                  geometry = TRUE)

write_rds (mobile, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/mobile.rds")
mobile <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/mobile.rds")
mobile <- st_transform(mobile)

mobile <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/mobile.rds")
colnames(mobile)[4] <- "Percent"
mobpal <- colorNumeric(palette = "viridis", domain = mobile$Percent, reverse = TRUE)

mobile_map <- mobile %>% 
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ mobpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
              label = ~paste0(NAME, "</br>", variable, ": ", Percent, "%")) %>% 
  addLegend("topleft",
            pal = mobpal,
            values = ~ Percent,
            title = "Mobility",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)


# Grandchildren -----------------------------------------------------------
grand <- get_acs(geography = "county",
                 state = "VA",
                 county = fips_codes,
                 variables = c("Grandchildren" = "S1002_C02_004"),
                 survey = "acs5",
                 year = 2019,
                 geometry = TRUE)

write_rds (grand, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/grand.rds")
grand <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/grand.rds")
grand <- st_transform(grand)

grand <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/grand.rds")
colnames(grand)[4] <- "Percent"
grandpal <- colorNumeric(palette = "viridis", domain = grand$Percent, reverse = TRUE)

grand_map <- grand %>% 
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ grandpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
              label = ~paste0(NAME, "</br>", variable, ": ", Percent, "%")) %>% 
  addLegend("topleft",
            pal = grandpal,
            values = ~ Percent,
            title = "Grandchildren",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)


# Married -----------------------------------------------------------------
married <- get_acs(geography = "county",
                   state = "VA",
                   county = fips_codes,
                   variables = c("Married" = "S1201_C02_019"),
                   survey = "acs5",
                   year = 2019,
                   geometry = TRUE)

write_rds (married, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/married.rds")
married <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/married.rds")
married <- st_transform(married)


married <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/married.rds")
colnames(married)[4] <- "Percent"
marriedpal <- colorNumeric(palette = "viridis", domain = married$Percent, reverse = TRUE)

married_map <- married %>% 
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ marriedpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
              label = ~paste0(NAME, "</br>", variable, ": ", Percent, "%")) %>% 
  addLegend("topleft",
            pal = marriedpal,
            values = ~ Percent,
            title = "Married",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)


# Personal Transportation -------------------------------------------------
priv_trans <- get_acs(geography = "county",
                      state = "VA",
                      county = fips_codes,
                      variables = c("Married" = "S0802_C02_013"),
                      survey = "acs5",
                      year = 2019,
                      geometry = TRUE)

write_rds (priv_trans, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/priv_trans.rds")
priv_trans <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/priv_trans.rds")
priv_trans <- st_transform(priv_trans)

priv_trans <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/priv_trans.rds")
colnames(priv_trans)[4] <- "Percent"
priv_transpal <- colorNumeric(palette = "viridis", domain = priv_trans$Percent, reverse = TRUE)

priv_trans_map <- priv_trans %>% 
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ priv_transpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
              label = ~paste0(NAME, "</br>", variable, ": ", Percent, "%")) %>% 
  addLegend("topleft",
            pal = priv_transpal,
            values = ~ Percent,
            title = "Private Transportation",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)


# Public Transportation ---------------------------------------------------
pub_trans <- get_acs(geography = "county",
                     state = "VA",
                     county = fips_codes,
                     variables = c("Married" = "S0802_C04_013"),
                     survey = "acs5",
                     year = 2019,
                     geometry = TRUE)

write_rds (pub_trans, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/pub_trans.rds")
pub_trans <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/pub_trans.rds")
pub_trans <- st_transform(pub_trans)

pub_trans <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/pub_trans.rds")
colnames(pub_trans)[4] <- "Percent"
pub_transpal <- colorNumeric(palette = "viridis", domain = pub_trans$Percent, reverse = TRUE)

pub_trans_map <- pub_trans %>% 
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ pub_transpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
              label = ~paste0(NAME, "</br>", variable, ": ", Percent, "%")) %>% 
  addLegend("topleft",
            pal = pub_transpal,
            values = ~ Percent,
            title = "Public Transportation",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)


# Computer and Internet Access --------------------------------------------
compin <- get_acs(geography = "county",
                  state = "VA",
                  county = fips_codes,
                  variables = c("Married" = "S2802_C03_006"),
                  survey = "acs5",
                  year = 2019,
                  geometry = TRUE)

write_rds (compin, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/compin.rds")
compin <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/compin.rds")
compin <- st_transform(compin)

compin <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/compin.rds")
colnames(compin)[4] <- "Percent"
compinpal <- colorNumeric(palette = "viridis", domain = compin$Percent, reverse = TRUE)

compin_map <- compin %>% 
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ compinpal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
              label = ~paste0(NAME, "</br>", variable, ": ", Percent, "%")) %>% 
  addLegend("topleft",
            pal = compinpal,
            values = ~ Percent,
            title = "Computer with Internet Access",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

# No Computer -------------------------------------------------------------
nocomp <- get_acs(geography = "county",
                  state = "VA",
                  county = fips_codes,
                  variables = c("Married" = "S2802_C07_006"),
                  survey = "acs5",
                  year = 2019,
                  geometry = TRUE)

write_rds (nocomp, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/nocomp.rds")
nocomp <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/nocomp.rds")
nocomp <- st_transform(nocomp)

nocomp <- read_rds("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/nocomp.rds")
colnames(nocomp)[4] <- "Percent"
nocomppal <- colorNumeric(palette = "viridis", domain = nocomp$Percent, reverse = TRUE)

nocomp_map <- nocomp %>% 
  leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ nocomppal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0,
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3),
              label = ~paste0(NAME, "</br>", variable, ": ", Percent, "%")) %>% 
  addLegend("topleft",
            pal = nocomppal,
            values = ~ Percent,
            title = "No Computer",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

ontimegrad <- read_csv("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/on_time_graduation.csv")
write_excel_csv(ontimegrad, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/on_time_graduation.xlsx")


