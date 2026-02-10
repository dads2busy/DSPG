# find zip codes within the DC MSA

setwd("~/git/dspg20career/src/geography/")

library(data.table)
library(dplyr)

# read in the ZCTA (zip code tabulated area) to MSA relation file
zctas <- fread("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_cbsa_rel_10.txt")

# get ZCTAs in the DC Metro area
zips <- zctas %>% filter(CBSA==47900) %>% transmute(ZCTA5)

fwrite(zips,file="zips47900.txt")
