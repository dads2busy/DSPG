library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)

#pulling the data set from acs and then converting it to a csv file
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

hamp_ages <- get_acs(geography = "county", state = 51,
                     county = county_fips[1],
                     table = "B01001",
                     year = 20109)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "B01001",
                 year = 2019)
  hamp_ages <- rbind(hamp_ages, tmp)
}

#convert to csv and pick directory
write.csv(hamp_ages, file = "shinyapp/data/TableB01001FiveYearEstimates/hamp_age2019.csv")
#reading in the file
hamp_ages <- read.csv("shinyapp/data/TableB01001FiveYearEstimates/hamp_age2019.csv")

hamp_ages <- hamp_ages[,2:6]
county_pop <- hamp_ages %>% group_by(NAME) %>%
  slice(1)
county_pop <- county_pop[,4]

#Getting male estimates for each age group 
county_male <- hamp_ages %>%
  group_by(NAME) %>%
  slice(3:25)

#Getting female estimates for each age group (summing every county for that specific female age group)
county_female <- hamp_ages %>%
  group_by(NAME) %>%
  slice(27:49)

#assigning ID to merge female and male estimates to get overall estimates
county_male <- tibble::rowid_to_column(county_male, "ID")
county_female <- tibble::rowid_to_column(county_female, "ID")

county_ages <- merge(county_female, county_male, by = "ID")
county_ages <- mutate(county_ages, total = estimate.x + estimate.y)

#get the estimates put in the age groups(map data)
#under 18
county_under <- county_ages %>%
  group_by(NAME.y) %>%
  slice(1:4)
county_under2 <- county_under %>%
  group_by(NAME.y) %>%
  summarise(x=sum(total))
county_under2<- county_under2[,2]
#young adult
county_ya <- county_ages %>%
  group_by(NAME.y) %>%
  slice(5:9)
county_ya2 <- county_ya %>%
  group_by(NAME.y) %>%
  summarise(x=sum(total))
county_ya2 <- county_ya2[,2]
#adult
county_adult <- county_ages %>%
  group_by(NAME.y) %>%
  slice(10:12)
county_adult2 <- county_adult %>%
  group_by(NAME.y) %>%
  summarise(x=sum(total))
county_adult2 <- county_adult2[,2]
#middle age
county_ma <- county_ages %>%
  group_by(NAME.y) %>%
  slice(13:17)
county_ma2 <- county_ma %>%
  group_by(NAME.y) %>%
  summarise(x=sum(total))
county_ma2 <- county_ma2[,2]
#senior
county_senior <- county_ages %>%
  group_by(NAME.y) %>%
  slice(18:23)
county_senior2 <- county_senior %>%
  group_by(NAME.y) %>%
  summarise(x=sum(total))
county_senior2 <- county_senior2[,2]

#labeling
counties_label <- c("Chesapeake", "Franklin", "Hampton", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Suffolk", "Virginia Beach", "Williamsburg", "Gloucester", "Isle of Wight", "James City", "Mathews", "Southampton", "York")

#getting coordinates for the pie charts
lat <- c(36.690473, 36.683540, 37.046933, 37.123232, 36.903378, 37.130348, 36.878493, 36.714941, 36.792042,      
         37.267284, 37.405450, 36.901637, 37.311197, 37.470724, 36.720152, 37.242246)
lon <- c(-76.297654, -76.940148, -76.390236, -76.523771, -76.248186, -76.357799, -76.380289, -76.626346,
         -76.053855, -76.708205, -76.519133, -76.708161, -76.804677, -76.375820, -77.114512, -76.566393)

#format
general_county_alt <-cbind(county_under2, county_ya2, county_adult2, county_ma2, county_senior2, county_pop)
colnames(general_county_alt) <- c("a", "b", "c", "d", "e", "total")
general_county_alt <-  mutate(general_county_alt, under = a/total*100)
general_county_alt <-  mutate(general_county_alt, ya = b/total*100)
general_county_alt <-  mutate(general_county_alt, adult = c/total*100)
general_county_alt <-  mutate(general_county_alt, ma = d/total*100)
general_county_alt <-  mutate(general_county_alt, senior = e/total*100)
general_county_alt2 <- general_county_alt[,7:11]
general_county_alt2 <-  mutate(general_county_alt2, county = counties_label)
general_county_alt2 <- cbind(general_county_alt2, lon, lat)

#Getting coordinates for the Hampton Roads Region by pulling the geometry column from acs from a random table
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

coordinate_data <- get_acs(geography = "county", state = 51,
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
  coordinate_data <- rbind(coordinate_data, tmp)
}

#covert to rds file
write_rds(coordinate_data, file ="shinyapp/data/TableB01001FiveYearEstimates/coordinates.rds")

library(plotrix)
library(scatterpie)
library(sf)
library(readr)
library(plotly)
#labeling for graphing (A through E are the slices of the pie chart)
colnames(general_county_alt2) <- c("A", "B","C","D", "E", "county", "lon", "lat")

#Getting map data for counties in Hampton roads
coord_data <- read_rds("shinyapp/data/TableB01001FiveYearEstimates/coordinates.rds")
coord_data <- st_transform(coord_data)
coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
coordinates2 <- coordinates1[,6]

#adding a column of names that align with the coordinates
city <- c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
          "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
          "Williamsburg", "York")
coordinates2 <- mutate(coordinates2, Loc = city)

#Changing "Franklin" to "Franklin City"
coordinates2$Loc[coordinates2$Loc == "Franklin"] <- "Franklin City"

#Graph
age_map <- ggplot(coordinates2) +
  geom_sf() +
  geom_sf_label(aes(label=Loc,geometry = geometry), label.padding = unit(.5, "mm"), size =4, nudge_x=0.05, nudge_y = 0.1) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=13)) +
  geom_scatterpie(aes(x=lon, y=lat, group=county, r =0.05), data=general_county_alt2,
                  cols=LETTERS[1:5]) + 
  scale_fill_viridis_d(labels = c("Under 18", "Young Adult: 18 to 29", "Adult: 30 to 44",
                                  "Middle Age: 45 to 64","Senior: 65 and Older")) 
