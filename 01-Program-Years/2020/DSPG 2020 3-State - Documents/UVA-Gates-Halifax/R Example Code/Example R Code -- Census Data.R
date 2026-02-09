# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
# install.packages("ggmap")
# install.packages("ggrepel")
# install.packages("leaflet")
# install.packages("htmltools")
library(tidycensus)
library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(ggmap)
library(ggrepel)
library(leaflet)
library(htmltools)


######## Pull ACS 2014/18 data for basic Patrick County sociodemographics #################



### API key ------------------------------------------------------------------------

# Go to http://api.census.gov/data/key_signup.html to submit request for Census API key
# Type in API key received by email (key needs to be activated by link in email) in quotes below to be stored in the environment
tidycensus::census_api_key("6ab331a72be811f87cb0386c6584ff7b1435f7c2", overwrite = TRUE, install = TRUE)

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


### Pulling in Decennial Census data ------------------------------------------------

# Tables of relevance can be searached for on data.census.gov/

# select total population data table (P001001) from census
sf1vars_2010.vapop <- c("P001001")
# Get decennial data from census (FIPS state code for virginia = 51)
dec2010.vapop <- get_decennial(geography = "county", year = 2010, state = 51, sumfile = "sf1", 
                         variables = sf1vars_2010.vapop, geometry = TRUE, keep_geo_vars = TRUE, output = "wide")
# Sort by county name
dec2010.vapop <- dec2010.vapop %>% dplyr::arrange(NAME.x)

# Rename P001001 to 'TOTALPOP'
dec2010.vapop <- dec2010.vapop %>% dplyr::rename(TOTALPOP = P001001)


# Select variables from 2010 Decennial Census table PCT20: GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE 
sf1vars_2010 <- c("PCT020001",	"PCT020002",	"PCT020003",	"PCT020004",	"PCT020005",	"PCT020006",	
                  "PCT020007",	"PCT020008",	"PCT020009",	"PCT020010",	"PCT020011",	"PCT020012",	
                  "PCT020013",	"PCT020014",	"PCT020015",	"PCT020016",	"PCT020017",	"PCT020018",	
                  "PCT020019",	"PCT020020",	"PCT020021",	"PCT020022",	"PCT020023",	"PCT020024",	
                  "PCT020025",	"PCT020026",	"PCT020027",	"PCT020028",	"PCT020029",	"PCT020030",	
                  "PCT020031",	"PCT020032")                       

# Get data decennial data from census (FIPS state code for virginia = 51)
dec2010.grpqrt <- get_decennial(geography = "county", year = 2010, state = 51, sumfile = "sf1", 
                         variables = sf1vars_2010, geometry = TRUE, keep_geo_vars = TRUE, output = "wide")
# Sort by county name
dec2010.grpqrt <- dec2010.grpqrt %>% dplyr::arrange(NAME.x)


# Rename group quarters variables
dec2010.grpqrt <- dec2010.grpqrt %>% dplyr::rename(TOTAL_GRP_QRTS = PCT020001,
                                                 TOTAL_INSTITUTE = PCT020002,
                                                 TOTAL_CORRECTIONS_ADULTS = PCT020003,
                                                 FEDERAL_DETENTION = PCT020004,
                                                 FEDERAL_PRISON = PCT020005,
                                                 STATE_PRISON = PCT020006,
                                                 LOCAL_JAIL = PCT020007,
                                                 RESID_CORRECTIONS = PCT020008,
                                                 MILITARY_CONFINE = PCT020009,
                                                 TOTAL_JUEV_FACILITIES = PCT020010,
                                                 JUEV_GRP_HOME = PCT020011,
                                                 JUEV_TREAT_CENTER = PCT020012,
                                                 JEUV_CORRECTIONS = PCT020013)

# Create new combined variables for federal and state prisons; use drop = TRUE to drop geometry for calcualtion (sf objects will throw a non-numeric error for this function without dropping the non-numeric geos)
dec2010.grpqrt <- dec2010.grpqrt %>% dplyr::mutate(FEDSTATE_PRISON_POP = rowSums(.[,c('FEDERAL_DETENTION', 'FEDERAL_PRISON', 'STATE_PRISON'), drop = TRUE], na.rm = TRUE))

# Create new combined variables for local incarceration institutions (e.g., jails, municpal centers)
dec2010.grpqrt <- dec2010.grpqrt %>% dplyr::mutate(LOCAL_INCARCERATION_POP = rowSums(.[,c('LOCAL_JAIL', 'RESID_CORRECTIONS'), drop = TRUE], na.rm = TRUE))

# Create new combined variables for juevinille centers that are non-correctional (e.g. foster care)
dec2010.grpqrt <- dec2010.grpqrt %>% dplyr::mutate(FOSTER_CARE_POP = rowSums(.[,c('JUEV_GRP_HOME', 'JUEV_TREAT_CENTER'), drop = TRUE], na.rm = TRUE))

names(dec2010.grpqrt) # Get names of all variables in data frame for selection
# Select specfic variables for join below
dec2010.grpqrt.redux <- dec2010.grpqrt %>% dplyr::select(GEOID, TOTAL_GRP_QRTS, TOTAL_INSTITUTE, TOTAL_CORRECTIONS_ADULTS, FEDERAL_DETENTION, FEDERAL_PRISON, STATE_PRISON,
                                                         LOCAL_JAIL, RESID_CORRECTIONS, MILITARY_CONFINE, TOTAL_JUEV_FACILITIES, JUEV_GRP_HOME, JUEV_TREAT_CENTER, JEUV_CORRECTIONS, FEDSTATE_PRISON_POP,
                                                         LOCAL_INCARCERATION_POP, FOSTER_CARE_POP)
# Remove geometry
dec2010.grpqrt.redux <- dec2010.grpqrt.redux %>% st_set_geometry(NULL)

# Join total counts of county populations by group quarter counts (134 counties & cities)
joined_va_incar <- dec2010.vapop %>% dplyr::left_join(dec2010.grpqrt.redux, by = "GEOID")


# Create a quotiant for calculating per 1,000 population by taking actual population number and dividing by 1,000
joined_va_incar$TOTALPOP_QUOTIANT <- joined_va_incar$TOTALPOP / 1000


# Federal & State Prison Pop Rate per 1,000
joined_va_incar <- joined_va_incar %>% dplyr::mutate(FEDSTATE_per1000rate = FEDSTATE_PRISON_POP / TOTALPOP_QUOTIANT) 

# Local Incarceration Rate Per 1,000
joined_va_incar <- joined_va_incar %>% dplyr::mutate(LOCAL_INCAR_per1000rate = LOCAL_INCARCERATION_POP / TOTALPOP_QUOTIANT) 

# Foster Care Rate Per 1,000
joined_va_incar <- joined_va_incar %>% dplyr::mutate(FOSTER_CARE_per1000rate = FOSTER_CARE_POP / TOTALPOP_QUOTIANT) 


## Static choropleth map using ggplot2 ======

palette1 <- c("#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8", "#756bb1", "#54278f") # purple
palette1 <- c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15") # red

# Plot of just virginia and its coutnies/independent cities
#png(width = 1900, height = 1709, res = 150) # starts save to working directory
ggplot2::ggplot() +
  geom_sf(data = joined_va_incar, size = 0.2, fill = '#bcdcd2') +
  ggrepel::geom_text_repel(data = joined_va_incar, aes(label = NAME.x, geometry = geometry), 
                           stat = "sf_coordinates", min.segment.length = 0, force = 0.05, size = 3, fontface = "bold", direction = "both", max.iter = 5000, box.padding = 0.5,
                           segment.color = "red", segment.size =	0.5, segment.alpha = 0.5, arrow = arrow(length = unit(0.009, "npc"), type = "closed", ends = "last")) +
  #geom_sf_text_repel(aes(label = NAME.x), data = joined_va_incar, size = 2.5, colour = "black") +
  labs(title = "Virginia Counties and Independent Cities") +
  ggthemes::theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "none")
#dev.off() # ends saving process

# Plot of Federal and State Prison Numbers per 1,000 People by Virgina County
#png(width = 1900, height = 1709, res = 150) # starts save to working directory
ggplot2::ggplot() +
  geom_sf(data = joined_va_incar, size = 0.2, aes(fill = FEDSTATE_per1000rate)) +
  #ggrepel::geom_text_repel(data = joined_va_incar, aes(label = NAME.x, geometry = geometry), 
                           #stat = "sf_coordinates", min.segment.length = 0, force = 0.05, size = 3, fontface = "bold", direction = "both", max.iter = 5000, box.padding = 0.5,
                           #segment.color = "red", segment.size =	0.5, segment.alpha = 0.5, arrow = arrow(length = unit(0.009, "npc"), type = "closed", ends = "last")) +
  #geom_sf_text(aes(label = NAME.x), data = joined_va_incar, size = 2.5, colour = "black") +
  #geom_sf_text(aes(label = round(FEDSTATE_per1000rate, 0)), data = joined_va_incar, size = 2.5, colour = "black", fontface = "bold") +
  labs(title = "Federal and State Prison Numbers per 1,000 People by Virgina County \n Decennial Census 2010") +
  ggthemes::theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.25, 0.35)) +
  scale_fill_continuous(name = "Count Per 1,000", low = "#fee5d9", high = "#a50f15")
#dev.off() # ends saving process


#dec2010.vapop %>% dplyr::select(NAME.x, TOTALPOP) %>% dplyr::filter(NAME.x == "Greensville")
#dec2010.grpqrt %>% dplyr::select(NAME.x, FEDERAL_DETENTION, FEDERAL_PRISON, STATE_PRISON) %>% dplyr::filter(NAME.x == "Greensville")
#joined_va_incar %>% dplyr::select(NAME.x, FEDERAL_DETENTION, FEDERAL_PRISON, STATE_PRISON, FEDSTATE_per1000rate) %>% dplyr::filter(NAME.x == "Greensville")

# Plot of Local Incarcerated per 1,000 People by Virgina County
#png(width = 1900, height = 1709, res = 150) # starts save to working directory
ggplot2::ggplot() +
  geom_sf(data = joined_va_incar, size = 0.2, aes(fill = LOCAL_INCAR_per1000rate)) +
  #ggrepel::geom_text_repel(data = joined_va_incar, aes(label = NAME.x, geometry = geometry), 
                           #stat = "sf_coordinates", min.segment.length = 0, force = 0.05, size = 3, fontface = "bold", direction = "both", max.iter = 5000, box.padding = 0.5,
                           #segment.color = "red", segment.size =	0.5, segment.alpha = 0.5, arrow = arrow(length = unit(0.009, "npc"), type = "closed", ends = "last")) +
  #geom_sf_text(aes(label = NAME.x), data = joined_va_incar, size = 2.5, colour = "black") +
  #geom_sf_text(aes(label = round(LOCAL_INCAR_per1000rate, 0)), data = joined_va_incar, size = 2.5, colour = "black", fontface = "bold") +
  labs(title = "Local Incarcerated per 1,000 People by Virgina County \n Decennial Census 2010") +
  ggthemes::theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.25, 0.35)) +
  scale_fill_continuous(name = "Count Per 1,000", low = "#fee5d9", high = "#a50f15")
#dev.off() # ends saving process

# Plot of Children in Foster Care per 1,000 People by Virgina County 
#png(width = 1900, height = 1709, res = 150) # starts save to working directory
ggplot2::ggplot() +
  geom_sf(data = joined_va_incar, size = 0.2, aes(fill = FOSTER_CARE_per1000rate)) +
  #ggrepel::geom_text_repel(data = joined_va_incar, aes(label = NAME.x, geometry = geometry), 
                           #stat = "sf_coordinates", min.segment.length = 0, force = 0.05, size = 3, fontface = "bold", direction = "both", max.iter = 5000, box.padding = 0.5,
                           #segment.color = "red", segment.size =	0.5, segment.alpha = 0.5, arrow = arrow(length = unit(0.009, "npc"), type = "closed", ends = "last")) +
  #geom_sf_text(aes(label = NAME.x), data = joined_va_incar, size = 2.5, colour = "black") +
  geom_sf_text(aes(label = round(FOSTER_CARE_per1000rate, 2)), data = joined_va_incar, size = 2.5, colour = "black", fontface = "bold") +
  labs(title = "Children in Foster Care per 1,000 People by Virgina County \n Decennial Census 2010") +
  ggthemes::theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.25, 0.35)) +
  scale_fill_continuous(name = "Count Per 1,000", low = "lightgoldenrod1", high = "seagreen4")
#dev.off() # ends saving process



## Dynamic choropleth map using leaflet ========

joined_va_incar2 <- sf::st_as_sf(joined_va_incar, wkt = "geometry")
joined_va_incar2 <- sf::st_transform(joined_va_incar2, 4326)


# Local Incarceration Rate
pallette.pal1 <- colorNumeric(palette = c('#fee0d9', '#fdcc8a', '#fc8d59', '#d7301f'), 
                             domain = joined_va_incar2$LOCAL_INCAR_per1000rate, na.color = "black", alpha = FALSE, reverse = FALSE) # set OrRd 4-class colorbrewer
# Set up popup upon click
joined_va_incar2 <- joined_va_incar2 %>% mutate(popup = paste0('<strong>County or Ind. City</strong>: ',  NAME.x, '<br><strong>Local Incar. Rate</strong>: ', htmltools::htmlEscape(format(LOCAL_INCAR_per1000rate, big.mark = ",", digits = 2)))) # Create new column with popup info in it
popup.val1 <- as.list(joined_va_incar2$popup) # save popup information as a list

# Outputs choropleth of US with each zip code shape
lf1 <- leaflet::leaflet() %>% 
  leaflet::addProviderTiles(providers$Stamen.Toner, options = providerTileOptions(minZoom = 2, maxZoom = 9)) %>%
  leaflet::addPolygons(data = joined_va_incar2, weight = .5, fillColor = ~pallette.pal1(LOCAL_INCAR_per1000rate), color = 'black', 
                       highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7, bringToFront = TRUE), label = lapply(popup.val1, htmltools::HTML), 
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
  leaflet::addLegend(pal = pallette.pal1, values = joined_va_incar2$LOCAL_INCAR_per1000rate, position = "topleft", title = "Rate Per 1,000")

#htmlwidgets::saveWidget(lf1, file = "va_local_incar_rate_bycounty.html", selfcontained = TRUE) # save dynamic file as html file or use export option in R studio










### Teja's ACS Census data and other examples ------------------------------



#
# Select variables ------------------------------------------------------------------------
#

# Load all variable names
# load_variables(2018, "acs5", cache = TRUE)

# % population age 65+
# B01001_020:25 (male), B01001_044:49 (female) / B01001_001    
# % population age <=18
# B01001_003:006 (male), B01001_027:30 (female) / B01001_001
# % population Hispanic or Latino
# B03001_003 / B02001_001
# % population Black
# B02001_003 / B02001_001
# % population over age 25 without a BA degree
# B15003_002:021 / B15003_001
# % population in labor force that is unemployed
# B23025_005 / B23025_002
# % population living under 100% poverty line
# B17001_002 / B17001_001
# % population without health insurance
# (005 + 008 + 011 + 014 + 017 + 020 + 023 + 026 + 029 + 033 + 036 + 039 + 042 + 045 + 048 + 051 + 054 + 057) /  B27001_001

# Select variables
acsvars <- c(
  # age 65 + 
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",
  "B01001_001",
  # age <18
  "B01001_003", "B01001_004", "B01001_005", "B01001_006",
  "B01001_027", "B01001_028", "B01001_029", "B01001_030",
  # Hispanic
  "B03001_003", "B03001_001",
  # Black
  "B02001_003", "B02001_001",
  # Without BA
  "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007",
  "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013",
  "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019",
  "B15003_020", "B15003_021", "B15003_001",
  # Unemployed
  "B23025_005", "B23025_002",
  # In poverty
  "B17001_002", "B17001_001",
  # Without health insurance
  "B27001_005", "B27001_008", "B27001_011", "B27001_014", "B27001_017", "B27001_020", "B27001_023", 
  "B27001_026", "B27001_029", "B27001_033", "B27001_036", "B27001_039", "B27001_042", "B27001_045", 
  "B27001_048", "B27001_051", "B27001_054", "B27001_057", "B27001_001"
)


#
# Get data ------------------------------------------------------------------------
#

# Get data from 2014/18 5-year estimates for Patrick County (51141) at tract level 
data_tract <- get_acs(geography = "tract", state = 51, county = 141,
                      variables = acsvars,
                      year = 2018, survey = "acs5",
                      cache_table = TRUE, output = "wide", geometry = TRUE,
                      keep_geo_vars = TRUE)

# Get data from 2014/18 5-year estimates for Patrick County (51141) at block group level
data_bgrp <- get_acs(geography = "block group", state = 51, county = 141,
                     variables = acsvars,
                     year = 2018, survey = "acs5",
                     cache_table = TRUE, output = "wide", geometry = TRUE,
                     keep_geo_vars = TRUE)

#
# Calculate ------------------------------------------------------------------------
#

acs_tract <- data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.x,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B02001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100
)

acs_bgrp <- data_bgrp %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  BLKGRPCE = BLKGRPCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.x,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B02001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100
)


#
# Plots ------------------------------------------------------------------------
#

# Age 65 and over
min_age65 <- floor(min(acs_bgrp$age65))
max_age65 <- ceiling(max(acs_bgrp$age65))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = age65)) +
  labs(title = "Percent population age 65 and over\nby Census block group, 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fff7ec", high = "#7F0000",
                        limits = c(min_age65, max_age65), 
                        breaks = seq(min_age65, max_age65, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_age65.png", plot = last_plot())

### --------------------- Decennial Census Code -----------------------------

# Notes
# dataset = "sf3" indicates census long form, "sf1" is short form, https://dusp.mit.edu/sites/dusp.mit.edu/files/attachments/publications/working_with_acs_R_v_2.0.pdf page 18 (UGH!)
# SF1 includes population tables (identified with a ''P'') and housing tables (identified with an ''H'') shown down to various levels of geography.
# https://www.census.gov/data/developers/data-sets/decennial-census.2000.html


#
# Find variables (note: this takes forever): 2000 -------------------------------------------------------------------------
#

# allvars_long <- load_variables(year = 2000, dataset = "sf3", cache = TRUE)
# View(allvars_long)

# Black
# Total pop: P006001
# Black: P006003

# Hispanic
# Total pop: P007001
# Hispanic: P007010

# Less than HS
# All 25+: P037001
# Male less HS: P037003, P037004, P037005, P037006, P037007, P037008, P037009, P037010
# Female less HS: P037020, P037021, P037022, P037023, P037024, P037025, P037026, P037027

# In poverty
# All population for whom poverty status is determined: P087001
# Income below poverty: 	P087002

# Single parent v1
# Total households: P010001
# Male HH no wife, has own kids: P010012
# Female HH no husband, has own kids: P010015

# Single parent v2
# Total families: P015001
# Male HH no wife, own kids: P015010
# Female HH no husband, own kids: P015016

# Single parent v3
# All own children under 18: P016001
# Male HH no wife: P016011
# Female HH no husband: P016019

# Select variables
sf3vars_2000 <- c("P037001", "P037003", "P037004", "P037005", "P037006", "P037007", 
                  "P037008", "P037009", "P037010", "P037020", "P037021", "P037022", 
                  "P037023", "P037024", "P037025", "P037026", "P037027", # less than HS 
                  "P006001", "P006003",                                  # black
                  "P007001", "P007010",                                  # hispanic
                  "P087001", "P087002",                                  # in poverty
                  "P016001", "P016011", "P016019")                       # single parent

# Get data
dec2000 <- get_decennial(geography = "tract", year = 2000, state = 51, county = 059, sumfile = "sf3", 
                         variables = sf3vars_2000, geometry = TRUE, keep_geo_vars = TRUE, output = "wide")

# Clean
dec2000 <- dec2000 %>% mutate(
  lesshs = (P037003 + P037004 + P037005 + P037006 + P037007 + P037008 + P037009 + P037010 + 
              P037020 + P037021 + P037022 + P037023 + P037024 + P037025 + P037026 + P037027) / P037001,
  hispanic = P007010 / P007001,
  black = P006003 / P006001,
  inpoverty = P087002 / P087001,
  singleparent = (P016011 + P016019) / P016001)


#
# Find variables (note: this takes forever): 1990 -------------------------------------------------------------------------
#


# Note: all tables are named "population subjects" and I have no idea what the denominators are....

# allvars_long90 <- load_variables(year = 1990, dataset = "sf3", cache = TRUE)
# View(allvars_long90)

# Persons total: P0010001
# Families total: P0040001
# Households total: P0050001

# Black: P0080002

# Hispanic
# Total: P0100001
# Hispanic by race: P0120006, P0120007, P0120008, P0120009, P0120010

# Total: presumably households, P0050001
# Male HH no wife, with child: P0190003
# Female HH no husband, with child: P0190005

# Total: presumably population,  P0010001
# Education: P0570001, P0570002 (less than 9th and 12 no diploma)
# Education 2: P0590001, P0590002 (less than 9th and 12 no diploma)

# Incime below poverty: P1170013 to P1170024
# Incime below poverty: P1200008 to P1200014

# Select variables
sf3vars_1990 <- c("P0010001", "P0040001", "P0050001",                                     # person, family, household total
                  "P0080002",                                                             # black,
                  "P0100001", "P0120006", "P0120007", "P0120008", "P0120009", "P0120010",             # hispanic
                  "P0190003", "P0190005",                                                 # single parent
                  "P0570001", "P0570002",                                                 # education
                  "P1200008", "P1200009", "P1200010", "P1200011", "P1200012", "P1200013", "P1200014") # income below poverty)              

# Get data
dec1990 <- get_decennial(geography = "tract", year = 1990, state = 51, county = 059, sumfile = "sf3", variables = sf3vars_1990, 
                         geometry = TRUE, keep_geo_vars = TRUE, output = "wide")


# Clean
dec1990 <- dec1990 %>% mutate(
  lesshs = (P0570001 + P0570002) / P0010001,
  hispanic = (P0120006 + P0120007 + P0120008 + P0120009 + P0120010) / P0010001,
  black = P0080002 / P0010001,
  inpoverty = (P1200008 + P1200009 + P1200010 + P1200011 + P1200012 + P1200013 + P1200014) / P0050001,
  singleparent = (P0190003 + P0190005) / P0050001)


#
# Alternatives --------------------------------------------------------------------------------------------
#

# Lookup
# search_tablecontents(survey = "dec", years = 2000)

# Then you can look up labels with
# acs.lookup(endyear = 1990, dataset = "sf3", keyword = "EDUCATION")



### Extra code ---------------------------

scale_fill_continuous(name = "Count Per 1,000", low = "#fee5d9", high = "#a50f15",
                      limits = c(round(min(joined_va_incar$LOCAL_INCAR_per1000rate), 2), round(max(joined_va_incar$LOCAL_INCAR_per1000rate), 2)))









