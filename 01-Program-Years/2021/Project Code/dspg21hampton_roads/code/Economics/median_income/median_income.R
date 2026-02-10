library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
#get data for 2010-2019 just by switching the year in va_table() and the file name
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}
#converting to excel
va_income19 <- va_table("S1903", 2019)
write.csv(va_income19, file = "shinyapp/data/TableS1903FiveYearEstimates/va_income2019.csv")

#hampton data
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
write.csv(hamp_income, file = "shinyapp/data/TableS1903FiveYearEstimates/hampton_income2019.csv")


#######################################################################VA Income
#2017-2019 fromat
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2019.csv")
va_yr <- va_yr[2:6]
#income by Race
race_names <- c("Total", "Black")
#median income
va_race_income_median <- data.frame(va_yr[c(81,83), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")

##################################################################Hampton Income
#using 2019 data
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

############################################################Va and Hampton Roads
#Putting them in the same datatset
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,4)]
#having all the estimates in the same column
median_income2 <- data.frame(median = c(median_income[,1], median_income[,2]))

#labeling
median_income2 <- mutate(median_income2, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income2 <- mutate(median_income2, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income2 <- transform(median_income2, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
#ordering
median_income2$Location <- factor(median_income2$Location, levels= c("Hampton Roads","Virginia"))

#Hampton and VA graph (for 2019)
income_plot <- ggplot(median_income2, aes(x=Location, y=`Median Income (US Dollars)`, fill=Demographic)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=paste0(round(`Median Income (US Dollars)`))), vjust=1.5, color="white",
            position = position_dodge(0.9), size=5)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        legend.text = element_text(size=14),
        axis.text=element_text(size=15),
        axis.title=element_text(size=17),
        axis.title.x=element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_fill_viridis_d()



#2010 -2016 format
#using 2016
va_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/va_income2016.csv")
va_yr <- va_yr[,2:6]
#income by Race
race_names <- c("Total", "Black")
#median income
va_race_income_median <- data.frame(va_yr[c(31,33), 4])
va_race_income <- data.frame(cbind(race_names, va_race_income_median))
colnames(va_race_income) <- c("Race", "Median Income")

##################################################################Hampton Income
#using 2010 data
hamp_yr <- read.csv("shinyapp/data/TableS1903FiveYearEstimates/hampton_income2016.csv")
hamp_yr <- hamp_yr[,2:6]
#getting the name, variable and estimate
hamp_income2 <- hamp_yr[,2:4]
hamp_income3 <- hamp_income2 %>%
  group_by(NAME) %>%
  slice(c(31,33))

#This give us overall hampton overall and black median income
variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))

############################################################Va and Hampton Roads
#Putting them in the same data set
median_income <- cbind(va_race_income, hamp_race_income_median)
median_income <- median_income[, c(2,6)]
#having all the estimates in the same column
median_income2 <- data.frame(median = c(median_income[,1], median_income[,2]))

#labeling
median_income2 <- mutate(median_income2, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
median_income2 <- mutate(median_income2, demo = rep(c("Total Population", "Black Population"),2))
colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
#making them all numeric
median_income2 <- transform(median_income2, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
#ordering
median_income2$Location <- factor(median_income2$Location, levels= c("Hampton Roads","Virginia"))

#Hampton and VA graph (for 2016)
income_plot <- ggplot(median_income2, aes(x=Location, y=`Median Income (US Dollars)`, fill=Demographic)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=paste0(round(`Median Income (US Dollars)`))), vjust=1.5, color="white",
            position = position_dodge(0.9), size=5)+
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'),
        legend.text = element_text(size=14),
        axis.text=element_text(size=15),
        axis.title=element_text(size=17),
        axis.title.x=element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_fill_viridis_d()

