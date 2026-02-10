library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)

#Age  for VA
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}

age1 <- va_table("B01001", 2010)

#converting to csv
write.csv(age1, file = "shinyapp/data/TableB01001FiveYearEstimates/va_age2010.csv")
#reading in csv
age1 <- read.csv("shinyapp/data/TableB01001FiveYearEstimates/va_age2010.csv")
age1 <- age1[,2:6]

#total population in VA: 8454463
va_total_pop<- age1[1,4]
#Adds the female and male data together to get the population for each age group
va_male_age <- age1[3:25,]
va_female_age <- age1[27:49,]
va_male_age <- tibble::rowid_to_column(va_male_age, "ID")
va_female_age <- tibble::rowid_to_column(va_female_age, "ID")
#adding the male and female estimates to get the total
ages <- merge(va_female_age, va_male_age, by = "ID")
ages <- mutate(ages, total = estimate.x + estimate.y)
#Getting just the estimates for each age group and transposing it to combining rows easily
va_ages1 <- data.frame(t(ages[,12]))
#Groups: Under 18, 18-30, 30-45, 45-65, 65+
va_ages1 <- mutate(va_ages1, Under18 = X1 + X2 + X3 + X4)
va_ages1 <- mutate(va_ages1, YoungAdult = X5 + X6 + X7 + X8 + X9)
va_ages1 <- mutate(va_ages1, Adult = X10 + X11 + X12)
va_ages1 <- mutate(va_ages1, MiddleAge = X13 + X14 + X15 + X16 + X17)
va_ages1 <- mutate(va_ages1, Senior = X18 + X19 + X20 + X21 + X22 + X23)
#using the 5 age group data
va_ages2 <- va_ages1[,24:28]
row.names(va_ages2) <- "Estimate"
va_ages2 <- data.frame(t(va_ages2))
va_ages2 <- mutate(va_ages2,TotalPopulation = va_total_pop)
#Make Percentage
va_ages2 <- mutate(va_ages2, PctPop = Estimate/TotalPopulation*100)
#labeling
va_ages2 <- mutate(va_ages2, labels = c("Under 18: 17 and Younger", "Young Adult: 18 to 29", "Adult: 30 to 44",
                                        "Middle Age: 45 to 64","Senior: 65 and Older"))
colnames(va_ages2) <- c("Estimate", "Total Population", "Percent of Population", "Labels")
#ordering
va_ages2[,4] <- factor(va_ages2[,4], levels = c("Under 18: 17 and Younger", "Young Adult: 18 to 29", "Adult: 30 to 44",
                                                "Middle Age: 45 to 64","Senior: 65 and Older"))
#Graph
va_graph <- ggplot(va_ages2 , aes(x="", y=`Percent of Population`, fill=Labels)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + 
  theme_void() +
  #ggtitle("Virginia") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank()) +
  geom_text(aes(label = paste0(round(`Percent of Population`), "%")), position = position_stack(vjust=0.5), size=5, color = "white") +
  scale_fill_viridis_d()  

#Age for Hampton Roads
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

#General
hamp_ages <- get_acs(geography = "county", state = 51,
                     county = county_fips[1],
                     table = "B01001",
                     year = 2010)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "B01001",
                 year = 2010)
  hamp_ages <- rbind(hamp_ages, tmp)
}

#convert to csv
write.csv(hamp_ages, file = "shinyapp/data/TableB01001FiveYearEstimates/hamp_age2010.csv")
#read in csv
hamp_ages <- read.csv("shinyapp/data/TableB01001FiveYearEstimates/hamp_age2010.csv")
hamp_ages <- hamp_ages[,2:6]

#total population in hampton Roads (1713267)
hamp_pop_tbl <- hamp_ages %>%
  group_by(NAME) %>%
  slice(1)
hamp_pop <- colSums(hamp_pop_tbl[,4])

#Getting male estimates for each age group (summing every county for that specific male age group)
hamp_male <- hamp_ages %>%
  group_by(NAME) %>%
  slice(3:25)
hamp_male2 <- hamp_male %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))

#Getting female estimates for each age group (summing every county for that specific female age group)
hamp_female <- hamp_ages %>%
  group_by(NAME) %>%
  slice(27:49)
hamp_female2 <- hamp_female %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))

#putting the two estimate column from hamp-male and hamp-female into on data set and then making another 
#data set where it's juts on long column of all the estimates
hamp_gender <- cbind(hamp_male2, hamp_female2)
hamp_gender <- hamp_gender[,c(2,4)]
#hamp_gender2 <- data.frame(estimate = c(hamp_gender[,1], hamp_gender[,2]))
colnames(hamp_gender) <- c("male", "female")
hamp_gender <- mutate(hamp_gender, total = male+female)
#transposing just the estimates
hamp_ages2 <- data.frame(t(hamp_gender[,3]))
#sorting into the age groups
hamp_ages2 <- mutate(hamp_ages2, Under18 = X1 + X2 + X3 + X4)
hamp_ages2 <- mutate(hamp_ages2, YoungAdult = X5 + X6 + X7 + X8 + X9)
hamp_ages2 <- mutate(hamp_ages2, Adult = X10 + X11 + X12)
hamp_ages2 <- mutate(hamp_ages2, MiddleAge = X13 + X14 + X15 + X16 + X17)
hamp_ages2 <- mutate(hamp_ages2, Senior = X18 + X19 + X20 + X21 + X22 + X23)
#using just the 5 age group data that was just sorted
hamp_ages3 <- hamp_ages2[,24:28]
row.names(hamp_ages3) <- "General Estimate"
hamp_ages3 <- data.frame(t(hamp_ages3))
#Getting the percentage
hamp_ages3 <- mutate(hamp_ages3,TotalPopulation = hamp_pop)
hamp_ages3 <- mutate(hamp_ages3, PctPop = General.Estimate/TotalPopulation*100)
hamp_ages3 <- mutate(hamp_ages3, Labels =c("Under 18: 17 and Younger", "Young Adult: 18 to 29", "Adult: 30 to 44",                                       "Middle Age: 45 to 64","Senior: 65 and Older"))
#ordering the age grpups
hamp_ages3$Labels <- factor(hamp_ages3$Labels, levels=c("Under 18: 17 and Younger", "Young Adult: 18 to 29", "Adult: 30 to 44",
                                                        "Middle Age: 45 to 64","Senior: 65 and Older"))
#Graph
hamp_graph <- ggplot(hamp_ages3 , aes(x="", y=PctPop, fill=Labels)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + 
  #ggtitle("Hampton Roads") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13)) +
  geom_text(aes(label = paste0(round(PctPop), "%")), position = position_stack(vjust=0.5), size=5, color = "white") +
  theme_void() +
  scale_fill_viridis_d()

#outputs
va_graph
hamp_graph
