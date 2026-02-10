library(tidycensus)
library(tidyverse)
library(dplyr)
#to update for 2020 just change the year in the function and file name
#to update the pie charts make va_age2020 hamp_age2020 make an if else statement for them (also update the drop down menu by just adding "2020" in the list of years on the ui end)
#to update the map use the newly made hamp_age2020 file in a new if else statement (also alter the drop down menu)

#Age  for VA
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}

age1 <- va_table("B01001", 2019)

#converting to csv
write.csv(age1, file = "shinyapp/data/TableB01001FiveYearEstimates/va_age2019.csv")


#Age for Hampton Roads
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

#General
hamp_ages <- get_acs(geography = "county", state = 51,
                     county = county_fips[1],
                     table = "B01001",
                     year = 2019)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "B01001",
                 year = 2019)
  hamp_ages <- rbind(hamp_ages, tmp)
}

#convert to csv
write.csv(hamp_ages, file = "shinyapp/data/TableB01001FiveYearEstimates/hamp_age2019.csv")