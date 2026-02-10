# BGT Data 
# DB Connect
conn <- RPostgreSQL::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                               dbname = "sdad",
                               host =  "postgis1",
                               port = 5432,
                               user = Sys.getenv("DB_USR"),
                               password = Sys.getenv("DB_PWD"))
# Reading in BGT Data 
bgt <- RPostgreSQL::dbGetQuery(conn = conn,
                               statement = "SELECT
                               onet,
                               soc,
                               maxdegree,
                               sector,
                               lat,
                               lon,
                               state, 
                               fipscounty,
                               id 
                               FROM bgt_job.main
                               WHERE jobdate >= '2019-01-01' AND jobdate <= '2019-12-31'")


# Reading in certification data                              
bgt_cert <- RPostgreSQL::dbGetQuery(conn = conn,
                                    statement = "SELECT id, jobdate, certification FROM bgt_job.cert WHERE jobdate >= '2019-01-01' AND jobdate <= '2019-12-31'")

# DB Disconnect
RPostgreSQL::dbDisconnect(conn)


# Libraries 
library(RPostgreSQL)
library(ggplot2)
library(readxl)
library(dplyr)
library(ggmap)
library(rgdal)

# Reading in STW SOC Codes 
stw <- read_xlsx("STW_2021 (1).xlsx")

# Transforming the STW data 
stw$SOC <- gsub("-", "", stw$SOC) # removing - in soc code 
stw$SOC <- as.integer(stw$SOC) # making soc code integer 
names(stw$SOC) <-"soc"
View(stw)

names(stw)[names(stw) == "SOC"] <- "soc"

# Transforming BGT data 
bgt$soc <- gsub("-", "", bgt$soc) # removing - in soc code 
bgt$soc <- as.integer(bgt$soc) # making soc code an integer  
library(plyr)
# Joining the data source bgt and stw 
merged <- merge(bgt, stw, by.x = "soc", by.y = "soc")  # merging on soc variable 
merged1 <- join(bgt, stw, by= "soc",)  # merging on soc variable 
# merged_no_na <- merged[is.na(merged$NAME) == FALSE,] # removing variables with no name 
colSums(merged)

# Joining the data source bgt_main (with no na's and no non-stw jobs) and bgt cert 
merged_cert <- merge(merged, bgt_cert, by.x = "id", by.y = "id") # merging on id variable 
bgt_stw_cred <- merged_cert[is.na(merged_cert$certification) == FALSE,] # removing jobs with no certification 

merged_cert$fam <- substr(merged_cert$soc,1,2 )


profile <- select(bgt_stw_cred, c(soc, lat, lon, state, certification))
profile <- profile[is.na(profile$lat) == FALSE,]
profile <- profile[is.na(profile$state) == FALSE,]

write.table(profile, file = "profile.txt", sep = ",")

SOC_frequencies <- table(profile$soc)
View(SOC_frequencies)

State_frequencies <- table(profile$fipsstate)
View(State_frequencies)

# Pulling soc and cred out for Emily 
# bgt_soc_cred <- select(bgt_stw_cred, c(soc,certification))
# write.table(bgt_soc_cred, file = "~/dspg21STW/src/bgt_soc_cred.txt", sep = ",")



bgt_profile <- read.csv("profile.txt")

# Profiling 

### Completness after manipulation 
# Count
sumNA <- colSums(is.na(merged))
sumNA
# Percentage
sumNA_percent <- 1 - (sumNA/nrow(merged_cert))
sumNA_percent

### Uniqueness after manipulation
# Count
sumUNI <- sapply(stw, n_distinct)
sumUNI
# Percentage
sumUNI_precent <- sumUNI/nrow(merged_cert)*100
sumUNI_precent

merged_cert[duplicated(merged_cert$id)]

z <- duplicated(merged_cert$id)
length(z[z== TRUE])

# SOC Codes 
SOC_frequencies_US <- table(bgt_profile$soc)
View(SOC_frequencies_US)

state_soc_frequency <- bgt_profile %>% 
  group_by(state) %>%
  count(soc, sort = TRUE) %>% 
  filter(dense_rank(-n) < 11)

stw_filtered <- stw %>% select(SOC, NAME)

state_frequency_table_labels <- merge(state_soc_frequency, stw_filtered, by.x = "soc", by.y = "SOC")


# Credential Data 
Cert_frequencies_US <- table(bgt_profile$certification)
View(Cert_frequencies_US)

state_cert_frequency <- bgt_profile %>% 
  group_by(state) %>%
  count(certification, sort = TRUE) %>% 
  filter(dense_rank(-n) < 11)


# Additional Graphs 
library(tigris)
library(leaflet)
library(tmap)
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
bgt.state <- bgt_profile %>% group_by(state) %>% summarise( number=n())

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
              border.alpha = 0, title = "BGT STW job ads per 10K people") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = "Spatial Distribution of BGT STW job ads",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")

state.map

# Graphs 

profile %>% 
  filter(lon <= min(USA_states$long) | lon >= max(USA_states$long))

profile_USA <- profile %>% 
  filter(!lon <= min(USA_states$long) & !lon >= max(USA_states$long), !is.na(lon))

prelim_plot <- ggplot(profile_USA, aes(x = lon, y = lat,color = as.factor(soc))) +geom_point()

prelim_plot + theme(legend.position = "none")

ggplot() + geom_polygon(data = USA_states, aes(x=long, y = lat, group = group), fill = NA, colour = "black") + 
  coord_fixed(1.3) +
  geom_point(data = profile_USA, aes(x=lon, y = lat, color = "red", alpha=1/15)) +
  theme_minimal() + theme(legend.position = "none") +
  labs(title = "The Locations of Credentials in the United States", x = "Longitude", y = "Latitude")


