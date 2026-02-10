library(readxl)
library(tidyverse)
library(readr)
library(maps)
library(housingData)
library(ggplot2)
library(gridExtra)
library(grid)
library(extrafont)
library(tidycensus)
#to update make sure the new kids count data is loaded in the suspension file under the file name shortTermSuspension.xlsx and then add another section (between the hash tags with 2019 format)
#and replace "2018-2019" to the new name of the column (probably either AY 2019-2020 or just 2019-2020) then add sus20 to gap_data and then copy the form at under "Changing the strings" comment for 2020
#do all of this in app.R. the code below just gives the set up of the code, so you can see how the graph would look before putting it in the app


#reading in the excel from KIDS Count
year <- "2018-2019"
suspension_data <- read_excel("shinyapp/data/suspension/shortTermSuspension.xlsx")
city <- c("Chesapeake", "Gloucester", "Hampton", "Isle of Wight", "James City", 
          "Newport News", "Norfolk", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
          "Williamsburg", "York")
suspension_counties <-filter(suspension_data, Location %in% city)

pct_white<- suspension_counties %>% filter(Race=="White") %>%
  filter(DataFormat=="Percent")
pct_white2 <- pct_white %>% filter(TimeFrame==year)
pct_white2$Location[pct_white2$Location == "Williamsburg"] <- "Williamsburg-James City"
pct_white2 <- pct_white2[c(1:4,6:13),]
pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
pct_white2 <- mutate(pct_white2, pct = Data *100)
#pct_white2$pct <- na_if(pct_white2$pct,0.00000)
as.numeric(pct_white2$pct)
#labeling
pct_white3 <- pct_white2[,c(2,7)]
colnames(pct_white3) <- c("Location", "Percentage of Students (%)")
#black data
suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
  filter(DataFormat=="Percent")
suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <- "Williamsburg-James City"
suspension_pct2 <- suspension_pct2[c(1:4,6:13),]
#convert data column to numeric so we can multiply by 100
suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
#suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
as.numeric(suspension_pct2$pct)
pct_blck <-suspension_pct2[,c(2,7)]
colnames(pct_blck) <- c("Location", "Percentage of Students (%)")###
sus <- merge(pct_blck,pct_white3, by = "Location")
sus <-mutate(sus, gap = sus[,2]- sus[,3])
sus19 <- mutate(sus[,c(1,4)], year = "2018-2019")
#########################################################################
year <- "AY 2017-2018"
suspension_data <- read_excel("shinyapp/data/suspension/shortTermSuspension.xlsx")
city <- c("Chesapeake", "Gloucester", "Hampton", "Isle of Wight", "James City", 
          "Newport News", "Norfolk", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
          "Williamsburg", "York")
suspension_counties <-filter(suspension_data, Location %in% city)

pct_white<- suspension_counties %>% filter(Race=="White") %>%
  filter(DataFormat=="Percent")
pct_white2 <- pct_white %>% filter(TimeFrame==year)
pct_white2$Location[pct_white2$Location == "Williamsburg"] <- "Williamsburg-James City"
pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
pct_white2 <- mutate(pct_white2, pct = Data *100)
as.numeric(pct_white2$pct)
#labeling
pct_white3 <- pct_white2[,c(2,7)]
colnames(pct_white3) <- c("Location", "Percentage of Students (%)")
#black data
suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
  filter(DataFormat=="Percent")
suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <- "Williamsburg-James City"

#convert data column to numeric so we can multiply by 100
suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
#suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
as.numeric(suspension_pct2$pct)
pct_blck <-suspension_pct2[,c(2,7)]
colnames(pct_blck) <- c("Location", "Percentage of Students (%)")###
sus <- merge(pct_blck,pct_white3, by = "Location")
sus <-mutate(sus, gap = sus[,2]- sus[,3])
sus18 <- mutate(sus[,c(1,4)], year = "2017-2018")
#############################################################################
year <- "AY 2016-2017"
suspension_data <- read_excel("shinyapp/data/suspension/shortTermSuspension.xlsx")
city <- c("Chesapeake", "Gloucester", "Hampton", "Isle of Wight", "James City", 
          "Newport News", "Norfolk", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
          "Williamsburg", "York")
suspension_counties <-filter(suspension_data, Location %in% city)

pct_white<- suspension_counties %>% filter(Race=="White") %>%
  filter(DataFormat=="Percent")
pct_white2 <- pct_white %>% filter(TimeFrame==year)
pct_white2$Location[pct_white2$Location == "Williamsburg"] <- "Williamsburg-James City"
pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
pct_white2 <- mutate(pct_white2, pct = Data *100)
as.numeric(pct_white2$pct)
#labeling
pct_white3 <- pct_white2[,c(2,7)]
colnames(pct_white3) <- c("Location", "Percentage of Students (%)")
#black data
suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
  filter(DataFormat=="Percent")
suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <- "Williamsburg-James City"

#convert data column to numeric so we can multiply by 100
suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
#suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
as.numeric(suspension_pct2$pct)
pct_blck <-suspension_pct2[,c(2,7)]
colnames(pct_blck) <- c("Location", "Percentage of Students (%)")###
sus <- merge(pct_blck,pct_white3, by = "Location")
sus <-mutate(sus, gap = sus[,2]- sus[,3])
sus17 <- mutate(sus[,c(1,4)], year = "2016-2017")
############################################################################
year <- "AY 2015-2016"
suspension_data <- read_excel("shinyapp/data/suspension/shortTermSuspension.xlsx")
city <- c("Chesapeake", "Gloucester", "Hampton", "Isle of Wight", "James City", 
          "Newport News", "Norfolk", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
          "Williamsburg", "York")
suspension_counties <-filter(suspension_data, Location %in% city)

pct_white<- suspension_counties %>% filter(Race=="White") %>%
  filter(DataFormat=="Percent")
pct_white2 <- pct_white %>% filter(TimeFrame==year)
pct_white2$Location[pct_white2$Location == "James City"] <- "Williamsburg-James City"
pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
pct_white2 <- mutate(pct_white2, pct = Data *100)
as.numeric(pct_white2$pct)
#labeling
pct_white3 <- pct_white2[,c(2,7)]
colnames(pct_white3) <- c("Location", "Percentage of Students (%)")
#black data
suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
  filter(DataFormat=="Percent")
suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
suspension_pct2$Location[suspension_pct2$Location == "James City"] <- "Williamsburg-James City"

#convert data column to numeric so we can multiply by 100
suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
#suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
as.numeric(suspension_pct2$pct)
pct_blck <-suspension_pct2[,c(2,7)]
colnames(pct_blck) <- c("Location", "Percentage of Students (%)")###
sus <- merge(pct_blck,pct_white3, by = "Location")
sus <-mutate(sus, gap = sus[,2]- sus[,3])
sus16 <- mutate(sus[,c(1,4)], year = "2015-2016")
################################################################################
year <- "AY 2014-2015"
suspension_data <- read_excel("shinyapp/data/suspension/shortTermSuspension.xlsx")
city <- c("Chesapeake", "Gloucester", "Hampton", "Isle of Wight", "James City", 
          "Newport News", "Norfolk", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
          "Williamsburg", "York")
suspension_counties <-filter(suspension_data, Location %in% city)

pct_white<- suspension_counties %>% filter(Race=="White") %>%
  filter(DataFormat=="Percent")
pct_white2 <- pct_white %>% filter(TimeFrame==year)
pct_white2$Location[pct_white2$Location == "James City"] <- "Williamsburg-James City"
pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
pct_white2 <- mutate(pct_white2, pct = Data *100)
as.numeric(pct_white2$pct)
#labeling
pct_white3 <- pct_white2[,c(2,7)]
colnames(pct_white3) <- c("Location", "Percentage of Students (%)")
#black data
suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
  filter(DataFormat=="Percent")
suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
suspension_pct2$Location[suspension_pct2$Location == "James City"] <- "Williamsburg-James City"

#convert data column to numeric so we can multiply by 100
suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
#suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
as.numeric(suspension_pct2$pct)
pct_blck <-suspension_pct2[,c(2,7)]
colnames(pct_blck) <- c("Location", "Percentage of Students (%)")###
sus <- merge(pct_blck,pct_white3, by = "Location")
sus <-mutate(sus, gap = sus[,2]- sus[,3])
sus15 <- mutate(sus[,c(1,4)], year = "2014-2015")
#combining
gap_data <- rbind(sus15, sus16, sus17, sus18, sus19)

#covert into a csv
write.csv(gap_data, file ="shinyapp/data/suspension/suspensionGap.csv")
#read csv
gap_data <- read.csv("shinyapp/data/suspension/suspensionGap.csv")
#Changing the strings
gap_data$year[gap_data$year == "2018-2019"] <- "2019"
gap_data$year[gap_data$year == "2017-2018"] <- "2018"
gap_data$year[gap_data$year == "2016-2017"] <- "2017"
gap_data$year[gap_data$year == "2015-2016"] <- "2016"
gap_data$year[gap_data$year == "2014-2015"] <- "2015"

colnames(gap_data) <- c("x", "Location", "Percent Difference", "Year")
#graph
susGapPlot <- ggplot(gap_data, aes(x=Year, y=`Percent Difference`, group =Location, color = Location)) + 
  geom_line(position = "identity", size =1.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=12),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    axis.text = element_text(siz=12)) +
  labs(y ="Percent Difference (%)", caption = "Source: KIDS COUNT, Annie E. Casey Foundation") +
  scale_color_viridis_d() +
  ylim(0,16)

#plot
suspensionGap <- ggplotly(susGapPlot, tooltip = c("x", "y", "group")) %>%
  layout(legend = list(y=0.5))

