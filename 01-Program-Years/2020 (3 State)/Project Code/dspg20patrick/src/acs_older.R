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

######## Pull ACS 2014/18 data for older adult variables #################

# API key ------------------------------------------------------------------------
#

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

# Select variables ------------------------------------------------------------------------
#

# Load all variable names
# load_variables(2018, "acs5", cache = TRUE)

# AGE
# % population age 65+
# B01001_020:25 (male), B01001_044:49 (female) / B01001_001   

#HEALTH INSURANCE
# % males 65+ without health insurance of all males 65+
# B27001_026 (M 65-74 no HI) + B27001_029 (M 75+ no HI ) / (B27001_024 (total M 65-74) + B27001_027 (total M 75+))

#% females 65+ without health insurance of all females 65+
#B27001_054 (F 65-74 no HI) + B27001_057 (F 75+ no HI) / B27001_052 (total F 65-74) + B27001_055 (total F 75+)

# % individuals 65+ without health insurance of all individuals 65+
# B27001_026 (M 65-74 no HI) + B27001_029 (M 75+ no HI ) + B27001_054 (F 65-74 no HI) + B27001_057 (F 75+ no HI) /  B27001_052 (total F 65-74) + B27001_055 (total F 75+) + (B27001_024 (total M 65-74) + B27001_027 (total M 75+))

#HEALTH STATUS
# % males 65+ with vision difficulty of all males 65+
# B18103_016 (M 65-74 visdiff) + B18103_019 (M 75+ visdiff) / B18103_015 (total M 65-74) + B18103_018 (total M 75+)

# % females 65+ with vision difficulty of all females 65+
# B18103_035 (F 65-74 visdiff) + B18103_038 (F 75+ visdiff) / B18103_034 (total F 65-74) + B18103_037 (total F 75+)

#% individuals 65+ with vision difficulty of all individuals 65+
# B18103_016 (M 65-74 visdiff) + B18103_019 (M 75+ visdiff) + B18103_035 (F 65-74 visdiff) + B18103_038 (F 75+ visdiff) / B18103_015 (total M 65-74) + B18103_018 (total M 75+) + B18103_034 (total F 65-74) + B18103_037 (total F 75+)

#% males 65+ with hearing difficulty of all males 65+
# B18102_016 (M 65-74 hdiff) + B18102_019 (M 75+ hdiff) / B18102_015 (total M 65-74) + B18102_018 (total M 75+)

#% females 65+ with hearing difficulty of all females 65+
# B18102_035 (F 65-74 hdiff) + B18102_038 (F 75+ hdiff) / B18102_034 (total F 65-74) + B18102_037 (total F 75+)

#% individuals 65+ with hearing difficulty of all individuals 65+
# B18102_016 (M 65-74 hdiff) + B18102_019 (M 75+ hdiff) + B18102_035 (F 65-74 hdiff) + B18102_038 (F 75+ hdiff) / B18102_015 (total M 65-74) + B18102_018 (total M 75+) + B18102_034 (total F 65-74) + B18102_037 (total F 75+)

#% males 65+ with cognitive difficulty of all males 65+
# B18104_013 (M 65-74 cdiff) + B18104_016 (M 75+ cdiff) / B18104_012 (total M 65-74) + B18104_015 (total M 75+)

#% females 65+ with cognitive difficulty of all females 65+
# B18104_029 (F 65-74 cdiff) + B18104_032 (F 75+ cdiff) / B18104_028 (total F 65-74) + B18104_031 (total F 75+)

#% individuals 65+ with cognitive difficulty of all individuals 65+
# B18104_013 (M 65-74 cdiff) + B18104_016 (M 75+ cdiff) + B18104_029 (F 65-74 cdiff) + B18104_032 (F 75+ cdiff) / B18104_012 (total M 65-74) + B18104_015 (total M 75+) + B18104_028 (total F 65-74) + B18104_031 (total F 75+)

#% males 65+ with ambulatory difficulty of all males 65+
# B18105_013 (M 65-74 adiff) + B18105_016 (M 75+ adiff) / B18105_012 (total M 65-74) + B18105_015 (total M 75+)

#% females 65+ with ambulatory difficulty of all females 65+
# B18105_029 (F 65-74 adiff) + B18105_032 (F 75+ adiff) / B18105_028 (total F 65-74) + B18105_031 (total F 75+)

#% individuals 65+ with ambulatory difficulty of all individuals 65+
# B18105_013 (M 65-74 adiff) + B18105_016 (M 75+ adiff) + B18105_029 (F 65-74 adiff) + B18105_032 (F 75+ adiff) / B18105_012 (total M 65-74) + B18105_015 (total M 75+) + B18105_028 (total F 65-74) + B18105_031 (total F 75+)

#% males 65+ with self-care difficulty of all males 65+
# B18106_013 (M 65-74 scdiff) + B18106_016 (M 75+ scdiff) / B18106_012 (total M 65-74) + B18106_015 (total M 75+)

#% females 65+ with self-care difficulty of all females 65+
# B18106_029 (F 65-74 scdiff) + B18106_032 (F 75+ scdiff) / B18106_028 (total F 65-74) + B18106_031 (total F 75+)

#% individuals 65+ with self-care difficulty of all individuals 65+
# B18106_013 (M 65-74 scdiff) + B18106_016 (M 75+ scdiff) + B18106_029 (F 65-74 scdiff) + B18106_032 (F 75+ scdiff) / B18106_012 (total M 65-74) + B18106_015 (total M 75+) + B18106_028 (total F 65-74) + B18106_031 (total F 75+)

#% males 65+ with independent living difficulty of all males 65+
# B18107_010 (M 65-74 ildiff) + B18107_013 (M 75+ ildiff) / B18107_009 (total M 65-74) + B18107_012 (total M 75+)

#% females 65+ with independent living difficulty of all females 65+
# B18107_023 (F 65-74 ildiff) + B18107_026 (F 75+ ildiff) / B18107_022 (total F 65-74) + B18107_025 (total F 75+)

#% individuals 65+ with independent living difficulty of all individuals 65+
# B18107_010 (M 65-74 ildiff) + B18107_013 (M 75+ ildiff) + B18107_023 (F 65-74 ildiff) + B18107_026 (F 75+ ildiff) / B18107_009 (total M 65-74) + B18107_012 (total M 75+) + B18107_022 (total F 65-74) + B18107_025 (total F 75+)

#% males 65+ with disability of all males 65+
# B18101_016 (M 65-74 dis + B18101_019 (M 75+ dis) / B18101_015 (total M 65-74) + B18101_018 (total M 75+)

#% females 65+ with disability of all females 65+
# B18101_035 (F 65-74 dis) + B18101_038 (F 75+ dis) / B18101_034 (total F 65-74) + B18101_037 (total F 75+)

#% individuals 65+ with disability of all individuals 65+
# B18101_016 (M 65-74 dis + B1801_019 (M 75+ dis) + B1801_035 (F 65-74 dis) + B1801_038 (F 75+ dis) / B18101_015 (total M 65-74) + BB18101_018 (total M 75+) + B1801_034 (total F 65-74) + B18101_037 (total F 75+)

#HARDSHIP
#% households with at least one 60+ household member receiving SNAP of all households with at least one 60+ member
#B22001_003 (At least one 60+, SNAP) / B22001_006 + B22001_003 (Total household with at least one 60+)

#% males 65+ with income below poverty (BP) level of all males 65+
# B17001_015 (M 65-74 BP) + B17001_016 (M 75+ BP) / (B17001_015 + B17001_016 + B17001_044 +B17001_045) (Total M 65+)

#% females 65+ with income below poverty level of all females 65+
#B17001_029 (F 65-74 BP) + B17001_030 (F 75+ BP) / (B17001_029 + B17001_030 + B17001_058 + B17001_059) (Total F 65+)

#% individuals 65+ with income below poverty level of all individuals 65+
#B17001_029 (F 65-74 BP) + B17001_030 (F 75+ BP) + B17001_015 (M 65-74 BP) + B17001_016 (M 75+ BP) / (B17001_015 + B17001_016 + B17001_029 + B17001_030 + B17001_044 +B17001_045 + B17001_058 + B17001_059) (Total 65+)

#HOUSEHOLDS
# sixty: % households with one or more 60+ member of all households
#B11006_002  (Households with one or more 60+) / B11006_001 (Total)

#% married couple families of all households with one or more 60+ member
#B11006_005(Married couple with one or more 60+)/ B11006_003 (Total family with 60+)

#% male householder with no wife of all households with one or more 60+ member
#B11006_006(M HH no wife 60+ present) /  B11006_003 (Total HH with 60+)

#% female householder with no husband of all households with one or more 60+ member
#B11006_007(F HH 60+ present) / B11006_003 (Total HH with 60+)

#% single (male OR female) householder with no wife/husband of all households with one or more 60+ member
#B11006_007(F HH 60+ present) + B11006_006(M HH no wife 60+ present) / B11006_003 (Total HH with 60+)

#% non family households of all households with one or more 60+ member
#B11006_008 (non-family HH 60+ present) / B11006_003 (Total HH with 60+)

#EMPLOYMENT
#% males 65+ in labor force (LF) of all males 65+
# B23001_074(M 65-69 LF) + B23001_079(M 70-74 LF) + B23001_084(M 75+ LF) / B23001_073(65-69 M) + B23001_078(70-74 M) + B23001_083(75+ M) (Total Males 65+)

# % females 65+ in labor force of all females 65+
#B23001_160(F 65-69 LF) + B23001_165(F 70-74 LF) + B23001_170(F 75+ LF) / B23001_159(F 65-69) + B23001_164(F 70 -74) + B23001_169(F 75+)

# % individuals 65+ in labor force of all individuals 65+
#B23001_160(F 65-69 LF) + B23001_165(F 70-74 LF) + B23001_170(F 75+ LF) + B23001_074(M 65-69 LF) + B23001_079(M 70-74 LF) + B23001_084(M 75+ LF) /  B23001_159(F 65-69) + B23001_164(F 70 -74) + B23001_169(F 75+) + B23001_073(65-69 M) + B23001_078(70-74 M) + B23001_083(75+ M) (All Individuals)

 # Select variables
acs_older_vars <- c(
  # Total older adults
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049", "B01001_001",
  # HI males age 65 +
  "B27001_024","B27001_026","B27001_027", "B27001_029",
  # HI females 65+
  "B27001_052","B27001_054","B27001_055" ,"B27001_057",
  # HEALTH STATUS
  "B18103_015", "B18103_016", "B18103_018", "B18103_019", "B18103_034", "B18103_035","B18103_037", "B18103_038", 
  "B18102_015", "B18102_016", "B18102_018", "B18102_019", "B18102_034", "B18102_035", "B18102_037", "B18102_038", 
  "B18104_012", "B18104_013", "B18104_015", "B18104_016", "B18104_028", "B18104_029", "B18104_031", "B18104_032", 
  "B18105_012", "B18105_013", "B18105_015", "B18105_016", "B18105_028", "B18105_029", "B18105_031", "B18105_032", 
  "B18106_012", "B18106_013", "B18106_015", "B18106_016", "B18106_028", "B18106_029", "B18106_031", "B18106_032", 
  "B18107_009", "B18107_010", "B18107_012", "B18107_013", "B18107_022", "B18107_023", "B18107_025", "B18107_026", 
  "B18101_015", "B18101_016", "B18101_018", "B18101_019", "B18101_034", "B18101_035", "B18101_037", "B18101_038",
  #SNAP Hardship
  "B22001_003","B22001_006",
  #BELOW POVERTY
  "B17001_015","B17001_016", "B17001_029", "B17001_030", "B17001_044", "B17001_045", "B17001_058", "B17001_059",
  #HOUSEHOLDS
  "B11006_001","B11006_002", "B11006_003","B11006_005","B11006_006", "B11006_007", "B11006_008",
  #EMPLOYMENT
  "B23001_073", "B23001_074", "B23001_078", "B23001_079", "B23001_083", "B23001_084", "B23001_159", "B23001_160", "B23001_164", 
  "B23001_165", "B23001_169", "B23001_170"
  )


#
# Get data ------------------------------------------------------------------------
#

# Get data from 2014/18 5-year estimates for Patrick County (51141) at tract level
older_data_tract <- get_acs(geography = "tract", state = 51, county = 141,
                      variables = acs_older_vars,
                      year = 2018, survey = "acs5",
                      cache_table = TRUE, output = "wide", geometry = TRUE,
                      keep_geo_vars = TRUE)

# Data not available at block group level except for total.


#
# Calculate ------------------------------------------------------------------------
#

# Tract level
acs_older_tract <- older_data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  # TOTAL OLDER ADULTS
  older = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
  B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  #HEALTH INSURANCE
  nohealthins_m = (B27001_026E + B27001_029E) / (B27001_024E + B27001_027E) * 100,
  nohealthins_f = (B27001_054E + B27001_057E) / (B27001_052E + B27001_055E) * 100,
  nohealthins = (B27001_026E + B27001_029E + B27001_054E + B27001_057E) / (B27001_052E + B27001_055E + B27001_024E + B27001_027E ) * 100,
  #HEALTH STATUS
  visdiff_m = (B18103_016E + B18103_019E) / (B18103_015E + B18103_018E) * 100,
  visdiff_f = (B18103_035E + B18103_038E) / (B18103_034E + B18103_037E) * 100,
  visdiff = (B18103_016E + B18103_019E + B18103_035E + B18103_038E) / (B18103_015E + B18103_018E + B18103_034E + B18103_037E) * 100,
  heardiff_m = (B18102_016E + B18102_019E) / (B18102_015E + B18102_018E) * 100,
  heardiff_f = (B18102_035E + B18102_038E) / (B18102_034E + B18102_037E) * 100,
  heardiff = (B18102_016E + B18102_019E + B18102_035E + B18102_038E) / (B18102_015E + B18102_018E + B18102_034E + B18102_037E) * 100,
  cogdiff_m = (B18104_013E + B18104_016E) / (B18104_012E + B18104_015E) * 100,
  cogdiff_f = (B18104_029E + B18104_032E) / (B18104_028E + B18104_031E) * 100,
  cogdiff = (B18104_013E + B18104_016E + B18104_029E + B18104_032E) / (B18104_012E + B18104_015E + B18104_028E + B18104_031E) * 100,
  ambdiff_m = (B18105_013E + B18105_016E) / (B18105_012E + B18105_015E) * 100,
  ambdiff_f = (B18105_029E + B18105_032E) / (B18105_028E + B18105_031E) * 100,
  ambdiff = (B18105_013E + B18105_016E + B18105_029E + B18105_032E) / (B18105_012E + B18105_015E + B18105_028E + B18105_031E) * 100,
  carediff_m = (B18106_013E + B18106_016E) / (B18106_012E + B18106_015E) * 100,
  carediff_f = (B18106_029E + B18106_032E) / (B18106_028E + B18106_031E) * 100,
  carediff = (B18106_013E + B18106_016E + B18106_029E + B18106_032E) / (B18106_012E + B18106_015E + B18106_028E + B18106_031E) * 100,
  ildiff_m = (B18107_010E + B18107_013E) / (B18107_009E + B18107_012E) * 100,
  ildiff_f = (B18107_023E + B18107_026E) / (B18107_022E + B18107_025E) * 100,
  ildiff = (B18107_010E + B18107_013E + B18107_023E + B18107_026E) / (B18107_009E + B18107_012E + B18107_022E + B18107_025E) * 100,
  disab_m = (B18101_016E + B18101_019E) / (B18101_015E + B18101_018E) * 100,
  disab_f = (B18101_035E + B18101_038E) / (B18101_034E + B18101_037E) * 100,
  disab = (B18101_016E + B18101_019E + B18101_035E + B18101_038E) / (B18101_015E + B18101_018E + B18101_034E + B18101_037E) * 100,
  #HARDSHIPS
  snap = B22001_003E / (B22001_006E + B22001_003E) * 100,
  inpov_m = (B17001_015E + B17001_016E) / (B17001_015E + B17001_016E + B17001_044E + B17001_045E) * 100,
  inpov_f = (B17001_029E + B17001_030E) / (B17001_029E + B17001_030E + B17001_058E + B17001_059E) * 100,
  inpov = (B17001_029E + B17001_030E + B17001_015E + B17001_016E) / (B17001_015E + B17001_016E + B17001_029E + B17001_030E + B17001_044E + B17001_045E + B17001_058E + B17001_059E) * 100,
  #HOUSEHOLDS
  hhsixty_total = B11006_002E / B11006_001E * 100,
  hhsixty_marr = B11006_005E / B11006_003E * 100,
  hhsixty_mhh = B11006_006E / B11006_003E * 100,
  hhsixty_fhh = B11006_007E / B11006_003E * 100,
  hhsixty_single = B11006_007E + B11006_006E / B11006_003E * 100,
  hhsixty_nonfam = B11006_008E / B11006_003E * 100,
  #EMPLOYMENT
  labfor_m = (B23001_074E + B23001_079E + B23001_084E) / (B23001_073E + B23001_078E + B23001_083E) * 100,
  labfor_f = (B23001_160E + B23001_165E + B23001_170E) / (B23001_159E + B23001_164E + B23001_169E) * 100,
  labfor = (B23001_160E + B23001_165E + B23001_170E + B23001_074E + B23001_079E + B23001_084E) / (B23001_159E + B23001_164E + B23001_169E + B23001_073E + B23001_078E + B23001_083E) * 100
  )


#
# Write out ------------------------------------------------------------------------
#

olderadults <- st_as_sf(acs_older_tract)
olderadults <- olderadults %>% st_transform(4269)
write_rds(olderadults, "./data/web/olderadults.Rds")


#
# Plots ------------------------------------------------------------------------
#

# Age 65 and over
min_healthins <- floor(min(acs_older_tract$nohealthins))
max_healthins <- ceiling(max(acs_older_tract$nohealthins))
ggplot() +
  geom_sf(data = acs_older_tract, size = 0.2, aes(fill = nohealthins)) +
  labs(title = "Percent population age 65 and over with no health insurance\nby Census tract, 2014/18",
       caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#efedf5", high = "#756bb1",
                        limits = c(min_healthins, max_healthins),
                        breaks = seq(min_healthins, max_healthins, length.out = 5))
ggsave(path = "./output/acs/", device = "png", filename = "plot_age65_nohi.png", plot = last_plot())

acs_plot <- function(acs_variables, plot_title, file_name, ...){
  ggplot() +
    geom_sf(data = acs_older_tract, size = 0.2, aes(fill = acs_variables)) +
    labs(title = sprintf("%s \nby Census tract, 2014/18", plot_title),
         caption = "Source: American Community Survey 2014/18 (5-year) estimates.") +
    theme_map() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.title = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 11),
          legend.position = "right") +
    scale_fill_continuous(name = "Percent", low = "#efedf5", high = "#756bb1",
                          limits = c(floor(min(acs_variables)), ceiling(max(acs_variables))),
                          breaks = seq(floor(min(acs_variables)), ceiling(max(acs_variables)), length.out = 5))
  ggsave(path = "./output/acs/", device = "png", filename = sprintf("plot_age65_%s.png", file_name), plot = last_plot())
  
}
acs_plot(acs_older_tract$nohealthins, "Percent older adults without health insurance", "nohealthins")
acs_plot(acs_older_tract$snap, "Percent households with one or more 60+\nhousehold members receiving SNAP", "snap")
acs_plot(acs_older_tract$inpov, "Percent older adults with income below poverty level", "inpov")
acs_plot(acs_older_tract$hhsixty_total, "Percent households with one or more 60+ members", "hhsixty_total")
acs_plot(acs_older_tract$visdiff, "Percent older adults with vision difficulty", "visdiff")
acs_plot(acs_older_tract$heardiff, "Percent older adults with hearing difficulty", "heardiff")
acs_plot(acs_older_tract$cogdiff, "Percent older adults with cognitive difficulty", "cogdiff")
acs_plot(acs_older_tract$ambdiff, "Percent older adults with ambulatory difficulty", "ambdiff")
acs_plot(acs_older_tract$carediff, "Percent older adults with self-care difficulty", "carediff")
acs_plot(acs_older_tract$ildiff, "Percent older adults with independent living difficulty", "ildiff")
acs_plot(acs_older_tract$disab, "Percent older adults with disability", "disab")
