# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("sf")
# install.packages("ggthemes")
# install.packages("RColorBrewer")
# install.packages("ggplot2")
library(tidycensus)
library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(leaflet)

######## Pull ACS 2014/18 data for basic Patrick County sociodemographics #################


#
# API key ------------------------------------------------------------------------
#

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


#
# Select variables ------------------------------------------------------------------------
#

# Load all variable names
# load_variables(2018, "acs5", cache = TRUE)

# total pop
# B01003_001
# % population age 65+
# B01001_020:25 (male), B01001_044:49 (female) / B01001_001    
# % population age <=18
# B01001_003:006 (male), B01001_027:30 (female) / B01001_001
# % population Hispanic or Latino
# B03001_003 / B03001_001
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
  # total pop
  "B01003_001",
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
  "B27001_048", "B27001_051", "B27001_054", "B27001_057", "B27001_001",
  # types of computers
  "B28001_001", "B28001_002", "B28001_003", "B28001_005","B28001_007", "B28001_011", "B28001_009",
  # presence and types of internet subscriptions
  "B28002_001", "B28002_013", "B28002_005", "B28002_009", "B28002_003", "B28002_007",
  # type of HI coverage by age, without HI
  "B27010_001", "B27010_017", "B27010_033", "B27010_050", "B27010_066",
  # public HI status by sex by age
  "B27003_001", "B27003_005", "B27003_008", "B27003_011", "B27003_014", "B27003_017", "B27003_020",
  "B27003_023", "B27003_026", "B27003_029", "B27003_033", "B27003_036", "B27003_039", "B27003_042",
  "B27003_045", "B27003_048", "B27003_051", "B27003_054", "B27003_057",
  # private HI status by sex by age
  "B27002_001", "B27002_005", "B27002_008", "B27002_011", "B27002_014", "B27002_017", "B27002_020",
  "B27002_023", "B27002_026", "B27002_029", "B27002_033", "B27002_036", "B27002_039", "B27002_042",
  "B27002_045", "B27002_048", "B27002_051", "B27002_054", "B27002_057",
  # public assistance or snap in past 12 months
  "B19058_001", "B19058_002"
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

# Tract level 
acs_tract <- data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  totalpop_trct = B01003_001E,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
           B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
             B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B03001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
          B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
          B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                 B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                 B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100,
  nocomputer = B28001_011E/B28001_001E * 100,
  laptop = B28001_003E/B28001_001E * 100,
  smartphone = B28001_005E/B28001_001E * 100,
  tablet = B28001_007E/B28001_001E * 100,
  othercomputer = B28001_009E/B28001_001E * 100,
  nointernet = B28002_013E/B28002_001E * 100,
  satellite = B28002_009E/B28002_001E * 100,
  cellular = B28002_005E/B28002_001E * 100,
  dialup = B28002_003E/B28002_001E * 100,
  broadband = B28002_007E/B28002_001E * 100,
  nohealthins2 = (B27010_017E + B27010_033E + B27010_050E + B27010_066E)/B27010_001E * 100,
  publicins = (B27003_005E + B27003_008E + B27003_011E + B27003_014E + B27003_017E + B27003_020E + B27003_023E + 
                 B27003_026E + B27003_029E + B27003_033E + B27003_036E + B27003_039E + B27003_042E + B27003_045E +
                 B27003_048E + B27003_051E + B27003_054E + B27003_057E) / B27003_001E * 100,
  privateins = (B27002_005E + B27002_008E + B27002_011E + B27002_014E + B27002_017E + B27002_020E + B27002_023E + 
                  B27002_026E + B27002_029E + B27002_033E + B27002_036E + B27002_039E + B27002_042E + B27002_045E +
                  B27002_048E + B27002_051E + B27002_054E + B27002_057E) / B27002_001E * 100,
  snap = B19058_002E/B19058_001E * 100
  )

# Block group (note: variables with estimate = 0 will have NAs in the final calculation. Disregard these
# for now and use tract-level values for plotting.)
acs_bgrp <- data_bgrp %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  BLKGRPCE = BLKGRPCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  totalpop_bgrp = B01003_001E,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B03001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100,
  nocomputer = B28001_011E/B28001_001E * 100,
  laptop = B28001_003E/B28001_001E * 100,
  smartphone = B28001_005E/B28001_001E * 100,
  tablet = B28001_007E/B28001_001E * 100,
  othercomputer = B28001_009E/B28001_001E * 100,
  nointernet = B28002_013E/B28002_001E * 100,
  satellite = B28002_009E/B28002_001E * 100,
  cellular = B28002_005E/B28002_001E * 100,
  dialup = B28002_003E/B28002_001E * 100,
  broadband = B28002_007E/B28002_001E * 100,
  nohealthins2 = (B27010_017E + B27010_033E + B27010_050E + B27010_066E)/B27010_001E * 100,
  publicins = (B27003_005E + B27003_008E + B27003_011E + B27003_014E + B27003_017E + B27003_020E + B27003_023E + 
                 B27003_026E + B27003_029E + B27003_033E + B27003_036E + B27003_039E + B27003_042E + B27003_045E +
                 B27003_048E + B27003_051E + B27003_054E + B27003_057E) / B27003_001E * 100,
  privateins = (B27002_005E + B27002_008E + B27002_011E + B27002_014E + B27002_017E + B27002_020E + B27002_023E + 
                  B27002_026E + B27002_029E + B27002_033E + B27002_036E + B27002_039E + B27002_042E + B27002_045E +
                  B27002_048E + B27002_051E + B27002_054E + B27002_057E) / B27002_001E * 100,
  snap = B19058_002E/B19058_001E * 100
)


#
# Plots ------------------------------------------------------------------------
#

# Age 65 and over
min_age65 <- floor(min(acs_bgrp$age65))
max_age65 <- ceiling(max(acs_bgrp$age65))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = age65)) +
  labs(title = "Percent population age 65 and over\nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                    limits = c(min_age65, max_age65), 
                    breaks = seq(min_age65, max_age65, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_age65.png", plot = last_plot())

# Age 18 and under
min_under18 <- floor(min(acs_bgrp$under18))
max_under18 <- ceiling(max(acs_bgrp$under18))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = under18)) +
  labs(title = "Percent population age 18 and under\nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_under18, max_under18), 
                        breaks = seq(min_under18, max_under18, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_under18.png", plot = last_plot())

# Hispanic
# only at tract level
min_hispanic <- floor(min(acs_tract$hispanic))
max_hispanic <- ceiling(max(acs_tract$hispanic))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = hispanic)) +
  labs(title = "Percent population Hispanic \nby Census tract, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_hispanic, max_hispanic), 
                        breaks = seq(min_hispanic, max_hispanic, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_hispanic.png", plot = last_plot())

# Black
min_black <- floor(min(acs_bgrp$black))
max_black <- ceiling(max(acs_bgrp$black))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = black)) +
  labs(title = "Percent population Black \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_black, max_black), 
                        breaks = seq(min_black, max_black, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_black.png", plot = last_plot())

# Age 25 and older and without a BA
min_noba <- floor(min(acs_bgrp$noba))
max_noba <- ceiling(max(acs_bgrp$noba))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = noba)) +
  labs(title = "Percent population 25 and over without a Bachelor's \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_noba, max_noba), 
                        breaks = seq(min_noba, max_noba, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_noba.png", plot = last_plot())

# Unemployed in LF
min_unempl <- floor(min(acs_bgrp$unempl))
max_unempl <- ceiling(max(acs_bgrp$unempl))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = unempl)) +
  labs(title = "Percent labor force population unemployed\nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_unempl, max_unempl), 
                        breaks = seq(min_unempl, max_unempl, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_unempl.png", plot = last_plot())

# At or Below 100 percent poverty level
# only at tract level
min_inpov <- floor(min(acs_tract$inpov))
max_inpov <- ceiling(max(acs_tract$inpov))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = inpov)) +
  labs(title = "Percent population in poverty \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_inpov, max_inpov), 
                        breaks = seq(min_inpov, max_inpov, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_inpov.png", plot = last_plot())

# No health insurance
# only at tract level
min_nohealthins <- floor(min(acs_tract$nohealthins))
max_nohealthins <- ceiling(max(acs_tract$nohealthins))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = nohealthins)) +
  labs(title = "Percent population without health insurance \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_nohealthins, max_nohealthins), 
                        breaks = seq(min_nohealthins, max_nohealthins, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_nohealthins.png", plot = last_plot())

# No Computer
min_nocomputer <- floor(min(acs_bgrp$nocomputer))
max_nocomputer <- ceiling(max(acs_bgrp$nocomputer))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = nocomputer)) +
  labs(title = "Percent population without a computer \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_nocomputer, max_nocomputer), 
                        breaks = seq(min_nocomputer, max_nocomputer, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_nocomputer.png", plot = last_plot())

# laptop
min_laptop <- floor(min(acs_bgrp$laptop))
max_laptop <- ceiling(max(acs_bgrp$laptop))
ggplot() +
                  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = laptop)) +
                  labs(title = "Percent population with a laptop \nby Census block group, 2014/18",
                       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
                  theme_map() +
                  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                        legend.title = element_text(size = 11, face = "bold"),
                        legend.text = element_text(size = 11),
                        legend.position = "right") +
                  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                                        limits = c(min_laptop, max_laptop), 
                                        breaks = seq(min_laptop, max_laptop, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_laptop.png", plot = last_plot())
                
# smartphone
min_smartphone <- floor(min(acs_bgrp$smartphone))
max_smartphone <- ceiling(max(acs_bgrp$smartphone))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = smartphone)) +
  labs(title = "Percent population with a smartphone \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_smartphone, max_smartphone), 
                        breaks = seq(min_smartphone, max_smartphone, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_smartphone.png", plot = last_plot())

# tablet
min_tablet <- floor(min(acs_bgrp$tablet))
max_tablet <- ceiling(max(acs_bgrp$tablet))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = tablet)) +
  labs(title = "Percent population with a tablet \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_tablet, max_tablet), 
                        breaks = seq(min_tablet, max_tablet, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_tablet.png", plot = last_plot())

# othercomputer
min_othercomputer <- floor(min(acs_bgrp$othercomputer))
max_othercomputer <- ceiling(max(acs_bgrp$othercomputer))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = othercomputer)) +
  labs(title = "Percent population another form of \na computer by Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_othercomputer, max_othercomputer), 
                        breaks = seq(min_othercomputer, max_othercomputer, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_othercomputer.png", plot = last_plot())

# nointernet
min_nointernet <- floor(min(acs_bgrp$nointernet))
max_nointernet <- ceiling(max(acs_bgrp$nointernet))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = nointernet)) +
  labs(title = "Percent population without internet \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_nointernet, max_nointernet), 
                        breaks = seq(min_nointernet, max_nointernet, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_nointernet.png", plot = last_plot())

# satellite
min_satellite <- floor(min(acs_bgrp$satellite))
max_satellite <- ceiling(max(acs_bgrp$satellite))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = satellite)) +
  labs(title = "Percent population with satellite internet \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_satellite, max_satellite), 
                        breaks = seq(min_satellite, max_satellite, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_satellite.png", plot = last_plot())

# cellular
min_cellular <- floor(min(acs_bgrp$cellular))
max_cellular <- ceiling(max(acs_bgrp$cellular))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = cellular)) +
  labs(title = "Percent population with cellular internet \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_cellular, max_cellular), 
                        breaks = seq(min_cellular, max_cellular, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_cellular.png", plot = last_plot())

# dialup
min_dialup <- floor(min(acs_bgrp$dialup))
max_dialup <- ceiling(max(acs_bgrp$dialup))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = dialup)) +
  labs(title = "Percent population with dial-up internet \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_dialup, max_dialup), 
                        breaks = seq(min_dialup, max_dialup, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_dialup.png", plot = last_plot())

# broadband
min_broadband <- floor(min(acs_bgrp$broadband))
max_broadband <- ceiling(max(acs_bgrp$broadband))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = broadband)) +
  labs(title = "Percent population with broadband internet \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_broadband, max_broadband), 
                        breaks = seq(min_broadband, max_broadband, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_broadband.png", plot = last_plot())

# nohealthins2
min_nohealthins2 <- floor(min(acs_bgrp$nohealthins2))
max_nohealthins2 <- ceiling(max(acs_bgrp$nohealthins2))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = nohealthins2)) +
  labs(title = "Percent population without health insurance \nby Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_nohealthins2, max_nohealthins2), 
                        breaks = seq(min_nohealthins2, max_nohealthins2, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_nohealthins2.png", plot = last_plot())

# privateins
# only at the tract level
min_privateins <- floor(min(acs_tract$privateins))
max_privateins <- ceiling(max(acs_tract$privateins))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = privateins)) +
  labs(title = "Percent population with private health insurance \nby Census tract, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_privateins, max_privateins), 
                        breaks = seq(min_privateins, max_privateins, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_privateins.png", plot = last_plot())

# publicins
# only at the tract level
min_publicins <- floor(min(acs_tract$publicins))
max_publicins <- ceiling(max(acs_tract$publicins))
ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = publicins)) +
  labs(title = "Percent popularion with public health insurance \nby Census tracr, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_publicins, max_publicins), 
                        breaks = seq(min_publicins, max_publicins, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_publicins.png", plot = last_plot())

# snap
min_snap <- floor(min(acs_bgrp$snap))
max_snap <- ceiling(max(acs_bgrp$snap))
ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = snap)) +
  labs(title = "Percent population with public assistance \nor snap benefits by Census block group, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_snap, max_snap), 
                        breaks = seq(min_snap, max_snap, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_snap.png", plot = last_plot())


#
# Write out ---------------------------------------------------------
#

# Connectivity
connectivity <- acs_bgrp %>% select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, GEOID,
                                    NAME.x, NAME.y, geometry,
                                    nocomputer, laptop, smartphone, tablet, othercomputer,
                                    nointernet, satellite, cellular, dialup, broadband)
connectivity <- st_as_sf(connectivity)
connectivity <- connectivity %>% st_transform(4269)
write_rds(connectivity, "./data/web/connectivity.Rds")

# Sociodemographics
socdem_block <- acs_bgrp %>% select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, GEOID,
                               NAME.x, NAME.y, geometry,
                               totalpop_bgrp, age65, under18, black, noba, unempl, nohealthins2, snap)
socdem_tract <- acs_tract %>% select(STATEFP, COUNTYFP, TRACTCE, GEOID,
                                    NAME.x, NAME.y, geometry,
                                    totalpop_trct, hispanic, inpov, privateins, publicins)

# Transform
socdem_block <- st_as_sf(socdem_block)
socdem_tract <- st_as_sf(socdem_tract)
socdem_block <- socdem_block %>% st_transform(4269)
socdem_tract <- socdem_tract %>% st_transform(4269)

# Write
write_rds(socdem_block, "./data/web/socdem_block.Rds")
write_rds(socdem_tract, "./data/web/socdem_tract.Rds")


#
# Leaflets ---------------------------------------------------------
#

# EXAMPLE: SNAP

acs_bgrp <- st_transform(acs_bgrp, '+proj=longlat +datum=WGS84')

pal <- colorQuantile("Blues", domain = acs_bgrp$snap, probs = seq(0, 1, length = 6), right = TRUE)

labels <- lapply(
  paste("<strong>Area: </strong>",
        acs_bgrp$NAME.y,
        "<br />",
        "<strong>% population with public assistance or SNAP benefits</strong>",
        round(acs_bgrp$snap, 2)),
  htmltools::HTML
)

leaflet(data = acs_bgrp, options = leafletOptions(minZoom = 10))%>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(snap), 
              fillOpacity = 0.6, 
              stroke = FALSE,
              label = labels,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))) %>%
  addMarkers(data = residential) %>%
  addLegend("bottomleft", 
            pal = pal, 
            values =  ~snap,
            title = "Percent by<br>Quintile Group", 
            opacity = 0.6,
            labFormat = function(type, cuts, p) {
              n = length(cuts)
              paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
            })


