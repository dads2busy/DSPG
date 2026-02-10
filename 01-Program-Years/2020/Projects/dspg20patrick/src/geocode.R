library(tidygeocoder)
library(dplyr)
library(readxl)
library(sf)
library(leaflet)
library(disco)
library(RColorBrewer)
library(readr)
library(stringr)
library(tigris)


#
# Read in --------------------------------------------------
#

groceries <- read_xlsx("./data/original/patrick_groceries.xlsx", col_names = TRUE, trim_ws = TRUE, progress = readxl_progress())
wifi <- read_xlsx("./data/original/patrick_wifi.xlsx", col_names = TRUE, trim_ws = TRUE, progress = readxl_progress())
otherfood <- read_xlsx("./data/original/patrick_otherfood.xlsx", col_names = TRUE, trim_ws = TRUE, progress = readxl_progress())


#
# Geocode --------------------------------------------------
#

groceries <- groceries %>% geocode(fulladdress, lat = latitude, long = longitude, method = "cascade")
wifi <- wifi %>% geocode(fulladdress, lat = latitude, long = longitude, method = "cascade")
otherfood <- otherfood %>% geocode(fulladdress, lat = latitude, long = longitude, method = "cascade")
  
# Patrick County High School, lat = 36.624179, long = -80.269961
wifi$latitude[5] <- 36.624179
wifi$longitude[5] <- -80.269961
wifi$geo_method[5] <- "manual"

# Patrick Henry Community College Stuart Campus, lat = 36.647241, long = -80.270562
wifi$latitude[10] <- 36.647241
wifi$longitude[10] <- -80.270562
wifi$geo_method[10] <- "manual"

# Meadows of Dan Food Market, lat = 36.735641, long = -80.407796
groceries$latitude[6] <- 36.735641
groceries$longitude[6] <- -80.407796
groceries$geo_method[6] <- "manual"

# Country Convenience Market, lat = 36.741524, long = -80.208900
groceries$latitude[10] <- 36.741524
groceries$longitude[10] <- -80.208900
groceries$geo_method[10] <- "manual"

# Meadows of Dan Community Building, lat = 36.735423, long = -80.403206
otherfood$latitude[5] <- 36.735423
otherfood$longitude[5] <- -80.403206
otherfood$geo_method[5] <- "manual"


#
# Plot --------------------------------------------------
#

patrickcty <- counties(state = "51", year = 2018)
patrickcty <- st_as_sf(patrickcty)
patrickcty <- patrickcty %>% filter(COUNTYFP == 141)

# Groceries
pal <- colorFactor(palette = disco(palette = "vibrant", n = 3), domain = groceries$type)
leaflet(groceries) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = patrickcty, stroke = T, weight = 2, color = "black", fillOpacity = 0) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 1, color = ~pal(type), radius = 4) %>%
  addLegend("bottomleft", 
            pal = pal, 
            values =  ~type,
            title = "Type", 
            opacity = 1)

# Wifi
leaflet(wifi) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = patrickcty, stroke = T, weight = 2, color = "black", fillOpacity = 0) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 1, radius = 6)

# Other food
pal <- colorFactor(palette = disco(palette = "vibrant", n = 3), domain = otherfood$type)
leaflet(otherfood) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = patrickcty, stroke = T, weight = 2, color = "black", fillOpacity = 0) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 1, color = ~pal(type), radius = 4) %>%
  addLegend("bottomleft", 
            pal = pal, 
            values =  ~type,
            title = "Type", 
            opacity = 1)


#
# Write out --------------------------------------------------
#

write.csv(groceries, "./data/working/geocode/patrick_groceries.csv")
write.csv(wifi, "./data/working/geocode/patrick_wifi.csv")
write.csv(otherfood, "./data/working/geocode/patrick_otherfood.csv")

groceries <- groceries %>% select(-notes)

write_rds(groceries, "./data/web/groceries.Rds")
write_rds(wifi, "./data/web/wifi.Rds")
write_rds(otherfood, "./data/web/otherfood.Rds")


#
# For web --------------------------------------------------
#

# Other food
otherfood$latitude <- jitter(otherfood$latitude, factor = 1)
otherfood$longitude <- jitter(otherfood$longitude, factor = 1)

pal <- colorFactor(c("#0E879C", "#D9E12B", "#E6A01D"), domain = otherfood$type)

labels <- lapply(
  paste("<strong>Name: </strong>",
        otherfood$name,
        "<br />",
        "<strong>Address:</strong>",
        otherfood$fulladdress,
        "<br />",
        "<strong>Type:</strong>",
        otherfood$type,
        "<br />",
        "<strong>Open to:</strong>",
        otherfood$audience,
        "<br />",
        "<strong>Notes:</strong>",
        otherfood$notes),
  htmltools::HTML
)

leaflet(data = otherfood, 
        options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = patrickcty, stroke = T, weight = 2, color = "black", fillOpacity = 0) %>%
  addCircleMarkers(stroke = FALSE, 
                   fillOpacity = 0.7, 
                   color = ~pal(type), 
                   radius = 7, 
                   opacity = 0.7,
                   label = labels, 
                   labelOptions = labelOptions(direction = "bottom",
                                               style = list(
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)",
                                                 direction = "auto"))) %>%
  addLegend("bottomleft", 
            pal = pal, 
            values =  ~type,
            title = "Type", 
            opacity = 0.7)
