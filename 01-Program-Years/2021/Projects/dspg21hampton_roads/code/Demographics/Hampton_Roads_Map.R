library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)

#taking the geometry column form a random acs table
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

coord_data <- get_acs(geography = "county", state = 51,
                      county = county_fips[1],
                      table = "B01001",
                      year = 2019,
                      geometry=TRUE)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "B01001",
                 year = 2019,
                 geometry=TRUE)
  coord_data <- rbind(coord_data, tmp)
}

#covert to rds file
write_rds(coord_data, file ="shinyapp/data/TableB01001FiveYearEstimates/coordinates.rds")
#read in rds file
coord_data <- read_rds("shinyapp/data/TableB01001FiveYearEstimates/coordinates.rds")

coord_data <- st_transform(coord_data)
coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
coordinates2 <- coordinates1[,6]

#Adding a column of the names tha align with coordinates
city <- c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
          "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
          "Williamsburg", "York")
coordinates2 <- mutate(coordinates2, Loc = city)

#Changing "Franklin" to "Franklin City"
coordinates2$Loc[coordinates2$Loc == "Franklin"] <- "Franklin City"

#Graph
hampton_counties_map <- ggplot(coordinates2) +
  geom_sf() +
  geom_sf_label(aes(label=Loc,geometry = geometry), label.padding = unit(.5, "mm"), size =4) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        panel.background = element_blank()) 
hampton_counties_map