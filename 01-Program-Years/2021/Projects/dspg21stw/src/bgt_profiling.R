##Profiling BGT - US


###################
#library
library(sf)
library(tidyverse)
library(tmap)
library(tidycensus)
library(tigris)
library(matrixStats)
library(SpatialAcc)
library(geojsonio)
library(rmapshaper)
library(sp)
library(ggplot2)
library(plotly)
library(leaflet)
##################

#complete the logging
conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host = "postgis1",
                               port = 5432,
                               user = "your_key",
                               password = "your_key")


bgt_2019 <- RPostgreSQL::dbGetQuery(
  conn = conn,
  statement = "
SELECT id, occfam , soc, socname, onet, employer, city, state, county, fips, minedu, mindegree, minexp, minsalary
FROM bgt_job.main
WHERE jobdate BETWEEN '2019-01-01' AND '2019-12-31'
  ")

# Disconnect
RPostgreSQL::dbDisconnect(conn)

#####################################################################

dim(bgt_2019)
names(bgt_2019)


#counts by state
bgt.state <- bgt_2019 %>% group_by(state) %>% summarise( number=n())
#quick overview
summary(bgt_2019)

###COMPLETENESS
#count NA per variable
sumNA <- colSums(is.na(bgt_2019))
#percentage of NA
sumNA.p <- sumNA/nrow(bgt_2019)*100

#UNIQUENESS
uni <- sapply(bgt_2019, n_distinct)


#for states
states <- states(cb = TRUE)

#obtain
pop.state <- get_acs(geography = "state",
                     year = 2019,
                     variables = c(tpop = "B01003_001E"
                                   , tpopr = "B03002_001E",
                                   nhwhite = "B03002_003E", nhblk = "B03002_004E",
                                   nhasn = "B03002_006E", hisp = "B03002_012E",
                                   medinc = "B19013_001E"
                     ),
                     survey = "acs5",
                     output = "wide",
                     geometry = TRUE)

#select variables
pop.state <- pop.state %>% select(GEOID, state=NAME, tpop, nhwhite, nhblk, nhasn, hisp, medinc,geometry   )

#join gbt numbers with pop to evaluate bgt add per capita
bgt.state <- bgt.state %>% left_join(pop.state, by = "state")
#bgt ads per person (per 10k)
bgt.state <- bgt.state %>% mutate( bgt.per1ok= number/tpop*10000 )

###########################
#map
#transform into readable format sf
bgt.state.utm <-st_as_sf(bgt.state)
#eliminate the NAs
bgt.state.utm.f <- subset(bgt.state.utm, !is.na(tpop))


tmap_mode("view")

state.map <- tm_shape(bgt.state.utm.f, unit = "mi") +
  tm_polygons(col = "bgt.per1ok", style = "jenks",palette = "Reds",
              border.alpha = 0, title = "BGT job ads per 10K people") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = "Spatial Distribution of BGT job ads",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")

state.map

########################################################################################################
#COUNTY
########################################################################################################


names(bgt_2019)

#counts by county
bgt.county.n <- bgt_2019 %>% group_by(fips) %>% summarise( number=n())

#obtain
pop.county <- get_acs(geography = "county",
                      year = 2019,
                      variables = c(tpop = "B01003_001E"
                                    , tpopr = "B03002_001E",
                                    nhwhite = "B03002_003E", nhblk = "B03002_004E",
                                    nhasn = "B03002_006E", hisp = "B03002_012E",
                                    medinc = "B19013_001E"
                      ),
                      survey = "acs5",
                      output = "wide",
                      geometry = TRUE)

pop.county <- pop.county %>% select(fips=GEOID, county=NAME, tpop, nhwhite, nhblk, nhasn, hisp, medinc,geometry   )

#join gbt numbers with pop to evaluate bgt add per capita
bgt.county <- bgt.county.n %>% left_join(pop.county, by = "fips")

bgt.county <- bgt.county %>% mutate( bgt.per1ok= number/tpop*10000 )

#plot

#map

#declare sf format
bgt.county.utm <-st_as_sf(bgt.county)
#eliminate the NAs
bgt.county.utm.f <- subset(bgt.county.utm, !is.na(tpop))


county.map <- tm_shape(bgt.county.utm.f, unit = "mi") +
  tm_polygons(col = "bgt.per1ok", style = "jenks",palette = "Reds",
              border.alpha = 0, title = "BGT job ads per 10K people") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = "Spatial Distribution of BGT job ads",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")






########################################################################################################
#LAB
########################################################################################################

#This graph no. for later

