library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)
library(olsrr)
library(stats)
library(psych)
library(viridis)
library(ggthemes)
library(ggmap)
library(ggspatial)
library(sf)
library(leaflet)
library(tigris)
library(readr)
library(hash)
library(readxl)
library(sos)
library(plotly)

setwd("~/GitPractice/DSPG2021_HamptonRoads")

#setting up the census key to retrieve ACS data (here you would use your own)
#installing it as well so you don't have to keep reloading the key when you run the whole file
census_api_key("5dff03cc06392730a33b6cc8b5f354730915dd20", install = TRUE)

#code for getting tables
#we used this table function to get the variable names in our function calls;
#we pulled up the ACS tables directly from the website, looked at our variable of interest, and once we loaded our table, by using 
#View(table), we were able to the get the variable name that corresponded to the value (each row had the variable and corresponding estimate to contrast online) on the table from ACS we wanted to use
va_table <- function(varcode, year) {
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode, year = year)) %>% drop_na()
}

#example of calling a table (this is our economics table for the different sectors seen below) and seeing the rows/values and extracting variable names
va_total2 = va_table("S1501", 2017)
View(va_total2)

#code for just getting one variable
va_variable <- function(varcode, year) {
  data.frame(get_acs(geography = "state", state = 51,
                     variable = varcode, year = year)) %>% drop_na()
}


#these are the varcodes we feed in to the calls for getting cities and counties of Hampton Roads
fips_codes <- c(550,620,650,700,710,735,740,800,810,830,73,93,95,115,175,199)


#getting ACS data for a selection of counties and for calculation of percentages
#varcode (or estimate) here is variable of interest, summary (or summary est) is population of interest for that variable we use
#to determine the percentages (estimate / summary est)
county_stats <- function(varcode, summary, year) { 
  get_acs(geography = "county",
                  county = fips_codes,
                  state = 51,
                  year = year,
                  variables = varcode, summary_var = summary) %>% 
  mutate(pct_tot = 100 * (estimate / summary_est)) %>% 
    select(NAME, variable, pct_tot) 
}


#this is same as the first function except for only one variable of interest, no calculation of percentages
#ACS has percentages that are retrievable in function calls for some years so had to use different functional calls
#for different years
county_stats1 <- function(varcode, year) { 
  get_acs(geography = "county",
          county = fips_codes,
          state = 51,
          year = year,
          variables = varcode) %>% 
    mutate(individualEstimates = estimate) %>% 
    select(NAME, variable, individualEstimates) 
}



#this is similar to the county stats 1 function but has the year column and Male column
#for black ed attainment data, it was given to us in male/female breakdown so we used three
#different function calls to get the male total numbers for ed attainment and same for females
#and then one for total; we could not do the individual estimate/summary estimate here like we did in the county stats function because
#it's two variables added together that are then over the total population, not just one over the total population. As a result,
#we had a function call for male, female, and total population, and then did the calculation ourselves on a separate column since 
#ACS did not have that data for the total Black population. We included in the variable column to show where we got each estimate (as well as in other function calls)

county_stats2 <- function(varcode, year) { 
  get_acs(geography = "county",
          county = fips_codes,
          state = 51,
          year = year,
          variables = varcode) %>% 
    mutate(Male = estimate) %>% 
    mutate(Year = year)  %>% 
    select(Year, NAME, variable, Male) 
}


#same deal with county stats 3 function but female estimates here; since we cbinded the data from the three different calls here,
#we didn't need to include the year column again for female and total estimates

county_stats3 <- function(varcode, year) { 
  get_acs(geography = "county",
          county = fips_codes,
          state = 51,
          year = year,
          variables = varcode) %>% 
    mutate(Female = estimate) %>% 
    mutate(variable2 = variable) %>% 
    select(variable2, Female) 
}


#same deal with county stats 4 function but total black population estimates here
county_stats4 <- function(varcode, year) { 
  get_acs(geography = "county",
          county = fips_codes,
          state = 51,
          year = year,
          variables = varcode) %>% 
    mutate(Total = estimate) %>% 
    mutate(variable3 = variable) %>% 
    select(variable3, Total)  
}


#adds in general black education
#these are the years ACS had five year estimates for
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) 

for (i in 1:length(years)) { 
  black_totalMale <- county_stats2("C15002B_006", years[i])
  black_totalFemale <- county_stats3("C15002B_011", years[i])
  black_totalBoth <- county_stats4("C15002B_001", years[i])
  black_total <- cbind(black_totalMale, black_totalFemale, black_totalBoth)
  #now we do calculation of black estimates within the same table
  black_total$BlackGeneral <- ((black_total$Male + black_total$Female)/black_total$Total) * 100
  
  #we write this to a csv for all the years
  #this is automated for all the years so we only have to call it once and loop over all the years
  #replace my directory with yours but keep the /shinyapp/data/... part the same, it is moving it to the actual shiny app data folder
  write_csv(black_total, file = paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
}

#we didn't end up using these datasets in the www folder but these are just the male/female contrasts (not the total black population) for the same table
for (i in 1:length(years)) { 
  #plots general data for education
  
  va_total2 = county_stats(c("C15002B_006", "C15002B_011"), "C15002B_001", years[i])
  gender <- rep(c("Male", "Female"), 16)
  va_total2 <- cbind(va_total2, gender)
  write_csv(va_total2, file = paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableC15002BFiveYearEstimates/blackEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
  va_total2CSV <- read.csv(paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableC15002BFiveYearEstimates/blackEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
}


#writes general ed attainment data
for (i in 1:length(years)) { 
  #these years just directly had the percentages so no need to sum up the numbers over the estimate
  if (years[i] == "2017"|| years[i] == "2016" || years[i] == "2015") {
    va_total2 = county_stats1("S1501_C02_015", years[i])
  }
  
  #these years just directly had the percentages so no need to sum up the numbers over the estimate
  else if (years[i] == "2010"|| years[i] == "2011" || years[i] == "2012" || years[i] == "2013" || years[i] == "2014")  {
    va_total2 =  county_stats1("S1501_C01_015", years[i])
  }
  
  else {
    #these years we had to do estimate/summary estimate to get the percentages
    va_total2 <- county_stats("S1501_C01_015", "S1501_C01_006", years[i])
  }
  
  write_csv(va_total2, file = paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableS1501FiveYearEstimates/generalEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
}



#writes general data for employment sector estimates
for (i in 1:length(years)) { 
  employment_types  = county_stats1(c("DP03_0033", "DP03_0034", "DP03_0035", "DP03_0036", "DP03_0037", "DP03_0038", "DP03_0039", "DP03_0040", "DP03_0041", "DP03_0042", "DP03_0043", "DP03_0044", "DP03_0045"), years[i])
  sectors <- rep(c("Agriculture, forestry, fishing and hunting, and mining", "Construction", "Manufacturing", "Wholesale trade", "Retail trade", "Transportation and warehousing, and utilities", "Information", "Finance and insurance, and real estate and rental and leasing", "Professional, scientific, and management, and administrative and waste management services", "Educational services, and health care and social assistance", "Arts, entertainment, and recreation, and accommodation and food services", "Other services, except public administration", "Public administration"), 16)
  employment_types  <- cbind(employment_types, sectors)
  
  #here I am sorting by the top 2 industry sectors per county/city in Hampton ROads
  employment_types <- employment_types %>% 
    arrange(desc(individualEstimates))  %>% 
    group_by(NAME)  %>% slice(1:2)
  
  write_csv(employment_types, file = paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableDP03FiveYearEstimates/top2employmentSectors", toString((years[i])),  ".csv", sep = ""))
  
}




#this is a seperate excel sheet we are reading in, so had to define schools of interest I was filtering by
countyAndCities <- c("Chesapeake City Public Schools",
              "Franklin City Public Schools",
              "Hampton City Public Schools",
              "Newport News City Public Schools",
              "Norfolk City Public Schools",
              "Poquoson City Public Schools",
              "Portsmouth City Public Schools",
              "Suffolk City Public Schools",
              "Virginia Beach City Public Schools",
              "Williamsburg-James City County Public Schools",
              "Gloucester County Public Schools",
              "Isle of Wight County Public Schools",
              "Mathews County Public Schools",
              "Southampton County Public Schools",
              "York County Public Schools")

#reading in the original teacher racial breakdown sheet VDOE provided for us and filtering off of that to make our own dataset
teacherByRace <- read_excel("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/teacherByRace.xlsx")
teacherByRace <- as.data.frame(teacherByRace) 
#filer only the school districts that correspond to those in Hampton Roads
teacherByRace <- teacherByRace[which(teacherByRace$`Division Name` %in% countyAndCities), ]

#if you'd like to see the data frame
View(teacherByRace)



#here we do all of our calculations for each race specified by that race's count over the total population of teachers for the corresponding city/county and create new columns in the dataframe for it
teacherByRace$BlackProportions <- (teacherByRace$Black/teacherByRace$`Total Counts`) * 100
teacherByRace$AsianProportions <- (teacherByRace$Asian/teacherByRace$`Total Counts`) * 100
teacherByRace$HispanicProportions <- (teacherByRace$Hispanic/teacherByRace$`Total Counts`) * 100
teacherByRace$WhiteProportions <- (teacherByRace$White/teacherByRace$`Total Counts`) * 100
teacherByRace$AmericanIndianProportions<- (teacherByRace$`American Indian`/teacherByRace$`Total Counts`) * 100
teacherByRace$TwoOrMoreRacesProportions <- (teacherByRace$`Two or More Races`/teacherByRace$`Total Counts`) * 100
teacherByRace$HawaiianProportions <- teacherByRace$`Hawaiian`/teacherByRace$`Total Counts`
#remove city, county pubic schools label to keep graphs consistent
teacherByRace <- teacherByRace %>% mutate(`Division Name` = str_remove(`Division Name`, "County Public Schools")) %>% mutate(`Division Name` = str_remove(`Division Name`, "City Public Schools")) %>% mutate(`Division Name` = str_remove(`Division Name`, "City"))

#for whatever reason, York is duplicated in the final dataset we load into shiny so removing that last duplicate row for York (which is the last row) so we don't mess up the plotly graphs; could just as easily removed the last row but wanted to show why with this to make it more clear
teacherByRace <- teacherByRace[!duplicated(teacherByRace), ]
write_csv(teacherByRace, file = ("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/teacherByRacesBreakdown.csv"))
