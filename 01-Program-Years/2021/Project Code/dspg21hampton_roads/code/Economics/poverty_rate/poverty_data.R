library(tidycensus)
library(tidyverse)
library(dplyr)

#To update for 2020 just change the year in the function and file name (make a va_poverty2020 and a hamp_poverty2020)
#to update va and Hampton bar graph use the newly created hamp_poverty2020 in app.R add another if else statement and assign hamp_pov do the same for va_pov (va_pov and hamp_pov should
#be assigned to va_poverty2020 and hamp_poverty2020 inside the if else statement)
#to update county bar graph create a new hamp_poverty2020 file then in app.R add another if else statement and assign hamp_pov
#note: need to update drop down menu by going to ui and finding the set up for poverty and then adding "2020" to the list of years in the drop down menu
#to see how to update the gif go to poverty_gif.R

#VA data
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}

va_pov <- va_table("S1701", 2019)
write.csv(va_pov, file = "shinyapp/data/TableS1701FiveYearEstimates/va_poverty2019.csv")

#Hampton data
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

hamp_pov <- get_acs(geography = "county", state = 51,
                    county = county_fips[1],
                    table = "S1701",
                    year = 2019)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "S1701",
                 year = 2019)
  hamp_pov <- rbind(hamp_pov, tmp)
}
write.csv(hamp_pov, file = "shiny/data/TableS1701FiveYearEstimates/hamp_poverty2019.csv")
