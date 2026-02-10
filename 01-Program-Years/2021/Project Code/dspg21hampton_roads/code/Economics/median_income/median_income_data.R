library(tidycensus)
library(tidyverse)
library(dplyr)

#median income data------------------------------------------------------------
#To update for 2020 change the year of the function and the year in the file name(two separate files, one for Hampton and one for VA)
#to update bar graph in app.R add another if else statement for 2020 (set hamp_yr and va_yr to the two new 2020 files) (also add "2020" to the list of years in the drop down menu in the ui)
#to update line graph copy and paste a section divided by hash tags and replace the year with 2020 and add median_income20 to income_years (update drop down menu)

#VA data
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}
#converting to excel
va_income19 <- va_table("S1903", 2019)
write.csv(va_income19, file = "shinyapp/data/TableS1903FiveYearEstimates/va_income2019.csv")

#Hampton data
#fips code for all the counties and cities in Hampton Roads
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

hamp_income <- get_acs(geography = "county", state = 51,
                       county = county_fips[1],
                       table = "S1903",
                       year = 2010)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "S1903",
                 year = 2019)
  hamp_income <- rbind(hamp_income, tmp)
}
write.csv(hamp_income, file = "shinyapp/data/TableS1903FiveYearEstimates/hampton_income2019.csv")


