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
#and replace "2018-2019" to the new name of the column (probably either AY 2019-2020 or just 2019-2020) make sure to change 2019 to 2020 within the code
#then add va_suspension_race20 to suspension_line 
#do this in app.R also remember to adjust the drop down menu in the ui by adding "2020" to the list of years

#using the excel from the KIDS COUNT website for short term suspension
year <- "2018-2019"
suspension_data <- read_excel("shinyap/data/suspension/kidsCountSuspension.xlsx")
#using only  VA data for 2018-2019
suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
#VA percentage estimate for 2018-2019 (Black)
va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (Hispanic)
va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (white)
va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
#combining the three percentages(black, hispanic, white)
va_suspension_race19 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
va_suspension_race19$Data <- as.numeric(va_suspension_race19$Data)
va_suspension_race19 <- mutate(va_suspension_race19, Data = Data*100)
va_suspension_race19 <- mutate(va_suspension_race19, race = c("Black", "Hispanic", "White"))
va_suspension_race19 <- mutate(va_suspension_race19, year = "2019")
######################################################################
year <- "AY 2017-2018"
suspension_data <- read_excel("shinyapp/data/suspension/kidsCountSuspension.xlsx")
#using only  VA data for 2018-2019
suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
#VA percentage estimate for 2018-2019 (Black)
va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (Hispanic)
va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (white)
va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
#combining the three percentages(b;ack, hispanic, white)
va_suspension_race18 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
va_suspension_race18$Data <- as.numeric(va_suspension_race18$Data)
va_suspension_race18 <- mutate(va_suspension_race18, Data = Data*100)
va_suspension_race18 <- mutate(va_suspension_race18, race = c("Black", "Hispanic", "White"))
va_suspension_race18 <- mutate(va_suspension_race18, year = "2018")
##########################################################################
year <- "AY 2016-2017"
suspension_data <- read_excel("shinyapp/data/suspension/kidsCountSuspension.xlsx")
#using only  VA data for 2018-2019
suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
#VA percentage estimate for 2018-2019 (Black)
va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (Hispanic)
va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (white)
va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
#combining the three percentages(b;ack, hispanic, white)
va_suspension_race17 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
va_suspension_race17$Data <- as.numeric(va_suspension_race17$Data)
va_suspension_race17 <- mutate(va_suspension_race17, Data = Data*100)
va_suspension_race17 <- mutate(va_suspension_race17, race = c("Black", "Hispanic", "White"))
va_suspension_race17 <- mutate(va_suspension_race17, year = "2017")
##############################################################################
year <- "AY 2015-2016"
suspension_data <- read_excel("shinyapp/data/suspension/kidsCountSuspension.xlsx")
#using only  VA data for 2018-2019
suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
#VA percentage estimate for 2018-2019 (Black)
va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (Hispanic)
va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (white)
va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
#combining the three percentages(b;ack, hispanic, white)
va_suspension_race16 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
va_suspension_race16$Data <- as.numeric(va_suspension_race16$Data)
va_suspension_race16 <- mutate(va_suspension_race16, Data = Data*100)
va_suspension_race16 <- mutate(va_suspension_race16, race = c("Black", "Hispanic", "White"))
va_suspension_race16 <- mutate(va_suspension_race16, year = "2016")
##############################################################################
year <- "AY 2014-2015"
suspension_data <- read_excel("shinyapp/data/suspension/kidsCountSuspension.xlsx")
#using only  VA data for 2018-2019
suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
#VA percentage estimate for 2018-2019 (Black)
va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (Hispanic)
va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
#VA percentage estimate for 2018-2019 (white)
va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
#combining the three percentages(b;ack, hispanic, white)
va_suspension_race15 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
va_suspension_race15$Data <- as.numeric(va_suspension_race15$Data)
va_suspension_race15 <- mutate(va_suspension_race15, Data = Data*100)
va_suspension_race15 <- mutate(va_suspension_race15, race = c("Black", "Hispanic", "White"))
va_suspension_race15 <- mutate(va_suspension_race15, year = "2015")
###########

#combined data
suspension_line <- rbind(va_suspension_race19, va_suspension_race18, va_suspension_race17, va_suspension_race16, va_suspension_race15)
colnames(suspension_line) <- c("Percent Students Suspended", "Race", "Year")

#graph
suspension_line_graph <- ggplot(suspension_line, aes(x=Year, y=`Percent Students Suspended`, group = Race, color = Race)) + 
  geom_line(position = "identity", size =1.8) +
  theme_minimal() +
  theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        axis.text = element_text(size=15)) +
  labs(y ="Percent of Students Suspended (%)") +
  scale_color_viridis_d() +
  scale_y_continuous(limits = c(2,14), breaks = seq(0, 14, by =2))

#plot
ggplotly(suspension_line_graph, tooltip = c("x", "y", "group")) %>%
layout(legend = list(y=0.5))



