# Installs and Loads  Required Packages-----------------------------------------------------
install.packages("tidycensus", "tidyverse", "dplyr", "tigris", "ggplot2", "sf", "leaflet", "RColorBrewer", "htmlwidgets")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(leaflet.providers)
library(RColorBrewer)
library(htmlwidgets)

div_fips <- c("136", "135", "036", "112", "046", "057", "117", "118", "142", "121", "087", "127", "128", "131", "098")


# Importing and Cleaning Data ---------------------------------------------

#County geo info
county_names <- c("Chesapeake", "Franklin", "Hampton", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Suffolk", 
                  "Virginia Beach", "Williamsburg", "Gloucester", "Isle of Wight", "James City", "Mathews", "Southampton", "York")

lat <- c(36.690473, 36.683540, 37.405450, 37.046933, 36.901637, 37.470724, 37.123232, 36.903378, 37.130348, 
         36.878493, 36.720152, 36.714941, 36.792042, 37.311197,  37.242246)

lon <- c(-76.297654, -76.940148, -76.519133, -76.390236, -76.708161, -76.375820, -76.523771, -76.248186, -76.357799, 
         -76.380289, -77.114512, -76.626346, -76.053855, -76.804677, -76.566393)


#County School Districts
div_fips <- c("136", "135", "036", "112", "046", "057", "117", "118", "142", "121", "087", "127", "128", "131", "098")

div_names <- c("Chesapeake City", "Franklin City", "Gloucester County", "Hampton City", "Isle of Wight County", "Mathews County", 
               "Newport News City", "Norfolk City", "Poquoson City", "Portsmouth City", "Southampton City", " Suffolk City", 
               "Virginia Beach City", "Williamsburg-James City County", "York County")

plot_data <- data.frame(div_names, lat, lon)

write_csv(plot_data, "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/dropoutmapdata.csv")

st_co <- read_csv("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/statelevelcohort.csv")

b_rates <- read_csv("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/cohort_stats_black.csv")
b_rates <- b_rates %>% 
  mutate(Race = "Black Students")

gen_rates <- read_csv("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/cohort_stats_general.csv")
gen_rates <- gen_rates %>% 
  mutate(Race = "All Students")

rates <- rbind(b_rates, gen_rates)

rates <- rates %>% 
  select(`Cohort Year`, `Division Name`, `Graduation Rate`, Race)

rates <- data.frame(rates)

clean_stco <- st_co[1:15,] %>% 
  select(`All Students`, `Black Students`) 

cleaner <- data.frame(t(clean_stco[3]))
colnames(cleaner) <- clean_stco[,2]

write_csv(clean_stco, "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/dropout2020.csv")

df2 <- data.frame(t(df[-1]))
colnames(df2) <- df[, 1]

basemap <- leaflet(width = "100%", height = "400px" ) %>% 
  addProviderTiles("CartoDB.Positron")

colors <- c("#0072B2", "#D55E00")

minmap <- basemap %>% 
  addMinicharts(
    plot_data$lon, plot_data$lat,
    chartdata = rates,
    colorPalette = colors,
    width = 45, height = 45
  )
  
minmap

  
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")


  



