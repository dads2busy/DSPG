library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(grid)
library(gridExtra)

#need to convert data from 2010-2019 
#VA data
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}
#converting to csv
va_income19 <- va_table("S1903", 2010)
write.csv(va_income19, file = "shinyapp/data/TableS1903FiveYearEstimates/va_income2010.csv")

#Hampton Roads data
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

#income by Race (just median)
hamp_income <- get_acs(geography = "county", state = 51,
                       county = county_fips[1],
                       table = "S1903",
                       year = 2010)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "S1903",
                 year = 2010)
  hamp_income <- rbind(hamp_income, tmp)
}
write.csv(hamp_income, file = "shinyapp/data/TableS1903FiveYearEstimates/hampton_income2010.csv")


#2017 to 2019 format
#############################################################################2019
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2019.csv")
va_yr <- va_yr[2:6]
race_names <- c("Total", "Black")
#median income
va_race_income_median <- data.frame(va_yr[c(81,83), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2019.csv")
hamp_yr <- hamp_yr[2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(81,83))
#This give us overall hampton overall and black median income
variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income19 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income19 <- mutate(median_income19, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income19 <- mutate(median_income19, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income19) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income19 <- transform(median_income19, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income19) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income19 <- mutate(median_income19, Year = "2019")
############################################################################2018
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2018.csv")
va_yr <- va_yr[2:6]
race_names <- c("Total", "Black")
#median income
va_race_income_median <- data.frame(va_yr[c(81,83), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2018.csv")
hamp_yr <- hamp_yr[2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(81,83))
#This give us overall hampton overall and black median income
variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income18 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income18 <- mutate(median_income18, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income18 <- mutate(median_income18, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income18) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income18 <- transform(median_income18, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income18) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income18 <- mutate(median_income18, Year = "2018")
############################################################################2017
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2017.csv")
va_yr <- va_yr[2:6]
race_names <- c("Total", "Black")
#median income
va_race_income_median <- data.frame(va_yr[c(81,83), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2017.csv")
hamp_yr <- hamp_yr[2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(81,83))
#This give us overall hampton overall and black median income
variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income17 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income17 <- mutate(median_income17, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income17 <- mutate(median_income17, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income17) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income17 <- transform(median_income17, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income17) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income17 <- mutate(median_income17, Year = "2017")

#2010-2016 format
###########################################################################2016
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2016.csv")
va_yr <- va_yr[,2:6]
race_names <- c("Total", "Black")
va_race_income_median <- data.frame(va_yr[c(31,33), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2016.csv")
hamp_yr <- hamp_yr[,2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(31,33))
variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income16 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income16 <- mutate(median_income16, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income16 <- mutate(median_income16, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income16) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income16 <- transform(median_income16, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income16) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income16 <- mutate(median_income16, Year = "2016")
###########################################################################2016
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2015.csv")
va_yr <- va_yr[,2:6]
race_names <- c("Total", "Black")
va_race_income_median <- data.frame(va_yr[c(31,33), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2015.csv")
hamp_yr <- hamp_yr[,2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(31,33))
variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income15 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income15 <- mutate(median_income15, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income15 <- mutate(median_income15, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income15) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income15 <- transform(median_income15, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income15) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income15 <- mutate(median_income15, Year = "2015")
###########################################################################2014
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2014.csv")
va_yr <- va_yr[,2:6]
race_names <- c("Total", "Black")
va_race_income_median <- data.frame(va_yr[c(31,33), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2014.csv")
hamp_yr <- hamp_yr[,2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(31,33))
variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income14 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income14 <- mutate(median_income14, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income14 <- mutate(median_income14, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income14) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income14 <- transform(median_income14, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income14) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income14 <- mutate(median_income14, Year = "2014")
###########################################################################2013
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2013.csv")
va_yr <- va_yr[,2:6]
race_names <- c("Total", "Black")
va_race_income_median <- data.frame(va_yr[c(31,33), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2013.csv")
hamp_yr <- hamp_yr[,2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(31,33))
variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income13 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income13 <- mutate(median_income13, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income13 <- mutate(median_income13, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income13) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income13 <- transform(median_income13, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income13) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income13 <- mutate(median_income13, Year = "2013")
############################################################################2012
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2012.csv")
va_yr <- va_yr[,2:6]
race_names <- c("Total", "Black")
va_race_income_median <- data.frame(va_yr[c(31,33), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2012.csv")
hamp_yr <- hamp_yr[,2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(31,33))
variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income12 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income12 <- mutate(median_income12, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income12 <- mutate(median_income12, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income12) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income12 <- transform(median_income12, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income12) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income12 <- mutate(median_income12, Year = "2012")
###########################################################################2011
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2011.csv")
va_yr <- va_yr[,2:6]
race_names <- c("Total", "Black")
va_race_income_median <- data.frame(va_yr[c(31,33), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2011.csv")
hamp_yr <- hamp_yr[,2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(31,33))
variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income11 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income11 <- mutate(median_income11, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income11 <- mutate(median_income11, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income11) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income11 <- transform(median_income11, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income11) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income11 <- mutate(median_income11, Year = "2011")
###########################################################################2010
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2010.csv")
va_yr <- va_yr[,2:6]
race_names <- c("Total", "Black")
va_race_income_median <- data.frame(va_yr[c(31,33), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")
#Hampton Income
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2010.csv")
hamp_yr <- hamp_yr[,2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(31,33))
variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
#Va and Hampton Roads
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income10 <- data.frame(median = c(median_income[,1], median_income[,2]))
#labeling
median_income10 <- mutate(median_income10, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income10 <- mutate(median_income10, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income10) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income10 <- transform(median_income10, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income10) <- c("Median Income (US Dollars)", "Location", "Demographic")
median_income10 <- mutate(median_income10, Year = "2010")

#########################################################################Combining all the years into one data set to graph
income_years <- rbind(median_income19, median_income18, median_income17, median_income16,
                      median_income15, median_income14, median_income13, median_income12,
                      median_income11, median_income10)
#VA line graph
va_years <- income_years %>% filter(Location=="Virginia")
#graph
va_line <- ggplot(va_years, aes(x=Year, y=`Median Income (US Dollars)`, group = Demographic, color = Demographic)) + 
  geom_line(position = "identity", size =1.3) +
  theme_minimal() +
  ggtitle("Virginia") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  labs(y ="Median Income (US Dollars)") +
  scale_color_manual(values = c("#D55E00", "#0072B2"))+
  ylim(35000, 75000)

#hamp line graph
hamp_years <- income_years %>% filter(Location == "Hampton Roads")
#graph
hamp_line <- ggplot(hamp_years, aes(x=Year, y=`Median Income (US Dollars)`, group = Demographic, color = Demographic)) + 
  geom_line(position = "identity", size =1.3 ) +
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  theme_minimal() +
  ggtitle("Hampton Roads")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  labs(y ="Median Income (US Dollars)")+
  ylim(35000, 75000)

#plot the two graphs together
time_graphs <- grid.arrange(hamp_line, va_line, ncol=2)
time_graphs