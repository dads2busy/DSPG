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
#to update make sure the new kids count data is loaded in the suspension file under the file name shortTermSuspension.xlsx 
#in app.R add another section (between the hash tags with 2019 format) and replace "2018-2019" to the new name of the column (probably either AY 2019-2020 or just 2019-2020) 
#make sure you add an if else statement and set year to 2019-2020 or AY 2019-2020. Also make sure to add "2020" to the list of years in the drop down menu in the ui
#do the above in app.R. The code below gives the set up of the code in the app and allows you to view the graph before putting it in the app


#2018-2019 format
year <- "2018-2019"
suspension_data <- read_excel("shinyapp/data/suspension/kidsCountSuspension.xlsx")
city <- c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews",
          "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
          "Williamsburg", "York")
suspension_counties <-filter(suspension_data, Location %in% city)

pct_white<- suspension_counties %>% filter(Race=="White") %>%
  filter(DataFormat=="Percent")
pct_white2 <- pct_white %>% filter(TimeFrame==year)
pct_white2$Location[pct_white2$Location == "Williamsburg"] <- "Williamsburg-James City"
pct_white2 <- pct_white2[c(1:5,7:16),]
#putting NAs and Ss in a table
pct_white2$Data[is.na(pct_white2$Data)] <- "NA"
display_tbl_white <- pct_white2 %>% filter(Data %in% c("NA", "S", "<", "*"))
display_tbl_white2<- display_tbl_white[,c(2,3,6)]
pct_white2$Data[pct_white2$Data=="NA"] <- 0
pct_white2$Data[pct_white2$Data=="S"] <- 0
pct_white2$Data[pct_white2$Data=="<"] <- 0
pct_white2$Data[pct_white2$Data=="*"] <- 0
#adding estimates by 100 (need to convert to numeric first)
pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
pct_white2 <- mutate(pct_white2, pct = Data *100)
pct_white2$pct <- na_if(pct_white2$pct,0.00000)
as.numeric(pct_white2$pct, na.rm = TRUE)
#labeling
pct_white3 <- pct_white2[,c(2,7)]
colnames(pct_white3) <- c("Location", "Percent (%)")
#black data
suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
  filter(DataFormat=="Percent")
suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <- "Williamsburg-James City"
suspension_pct2 <- suspension_pct2[c(1:5,7:16),]
#make a table w/ NA a S
suspension_pct2$Data[is.na(suspension_pct2$Data)] <- "NA"
display_tbl_black <- suspension_pct2 %>% filter(Data %in% c("NA", "S","<", "*"))
display_tbl_black2 <- display_tbl_black[,c(2,3,6)]
suspension_pct2$Data[suspension_pct2$Data=="NA"] <- 0
suspension_pct2$Data[suspension_pct2$Data=="S"] <- 0
suspension_pct2$Data[suspension_pct2$Data=="<"] <- 0
suspension_pct2$Data[suspension_pct2$Data=="*"] <- 0
#convert data column to numeric so we can multiply by 100
suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
as.numeric(suspension_pct2$pct, na.rm = TRUE)
pct_blck <-suspension_pct2[,c(2,7)]
colnames(pct_blck) <- c("Location", "Percent (%)")
sus <- rbind(pct_blck,pct_white3)
num <- nrow(sus)/2
sus <- mutate(sus, Race = c(rep("Black",num), rep("White", num)))
#bar graph
suspension_counties_plot <-
  ggplot(sus , aes(Location, y=`Percent (%)`, fill=Race)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=13),
        axis.title=element_text(size=17),
        axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.95, hjust=1))+
  scale_fill_manual(values=c("#D55E00","#0072B2")) +
  labs(x = "Location")

#plot
BW_map <- suspension_counties_plot
ggplotly(BW_map)



#not in the app, but tells which counties' data is supressed or NA
#combining the tables (not used but can be used via grid.arrange to see the counties with NA, S, etc)
display_table <- rbind(display_tbl_white2, display_tbl_black2)
na_rows <- display_table %>% filter(Data == "NA")
supr_rows <- display_table %>% filter(Data == "S")
less_rows <- display_table %>% filter(Data == "<")
other_rows <- display_table %>% filter(Data == "*")
supr_rows <- mutate(supr_rows, Data = "Suppressed")
other_rows <- mutate(other_rows, Data = "Suppressed")
less_rows <- mutate(less_rows, Data = "less than 10")
display_table_final <- rbind(na_rows, supr_rows, less_rows, other_rows)
table_plot <- tableGrob(display_table_final, rows = NULL)
#to see the table uncomment the line below
#grid.arrange(suspension_counties_plot, display_table)

#use the section above when getting 2020 (it's the updated format)-------------------------------------------
#the rest of the years format (just change the year variable to get a different year. It's in a loop in app.R)
year <- "AY 2014-2015"
suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
city <- c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews",
          "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
          "Williamsburg", "York")
suspension_counties <-filter(suspension_data, Location %in% city)

pct_white<- suspension_counties %>% filter(Race=="White") %>%
  filter(DataFormat=="Percent")
pct_white2 <- pct_white %>% filter(TimeFrame==year)
pct_white2$Location[pct_white2$Location == "James City"] <- "Williamsburg-James City"
pct_white2$Location[pct_white2$Location == "Williamsburg"] <- "Williamsburg-James City"
#putting NAs and Ss in a table
pct_white2$Data[is.na(pct_white2$Data)] <- "NA"
display_tbl_white <- pct_white2 %>% filter(Data %in% c("NA", "S", "<", "*"))
display_tbl_white2<- display_tbl_white[,c(2,3,6)]
pct_white2$Data[pct_white2$Data=="NA"] <- 0
pct_white2$Data[pct_white2$Data=="S"] <- 0
pct_white2$Data[pct_white2$Data=="<"] <- 0
pct_white2$Data[pct_white2$Data=="*"] <- 0
#adding estimates by 100 (need to convert to numeric first)
pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
pct_white2 <- mutate(pct_white2, pct = Data *100)
pct_white2$pct <- na_if(pct_white2$pct,0.00000)
as.numeric(pct_white2$pct, na.rm = TRUE)
#labeling
pct_white3 <- pct_white2[,c(2,7)]
colnames(pct_white3) <- c("Location", "Percentage of Students (%)")
#black data
suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
  filter(DataFormat=="Percent")
suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
suspension_pct2$Location[suspension_pct2$Location == "James City"] <- "Williamsburg-James City"
suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <- "Williamsburg-James City"
#make a table w/ NA a S
suspension_pct2$Data[is.na(suspension_pct2$Data)] <- "NA"
display_tbl_black <- suspension_pct2 %>% filter(Data %in% c("NA", "S","<", "*"))
display_tbl_black2 <- display_tbl_black[,c(2,3,6)]
suspension_pct2$Data[suspension_pct2$Data=="NA"] <- 0
suspension_pct2$Data[suspension_pct2$Data=="S"] <- 0
suspension_pct2$Data[suspension_pct2$Data=="<"] <- 0
suspension_pct2$Data[suspension_pct2$Data=="*"] <- 0
#convert data column to numeric so we can multiply by 100
suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
as.numeric(suspension_pct2$pct, na.rm = TRUE)
pct_blck <-suspension_pct2[,c(2,7)]
colnames(pct_blck) <- c("Location", "Percentage of Students (%)")

sus <- rbind(pct_blck,pct_white3)
num <- nrow(sus)/2
sus <- mutate(sus, Race = c(rep("Black",num), rep("White", num)))

#bar graph
suspension_counties_plot <-
  ggplot(sus , aes(Location, y=`Percentage of Students (%)`, fill=Race)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal() +
  ylab("Percent (%)")+
  theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(0.3, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.title.y = element_text(size=13),
        axis.text=element_text(size=12),
        #axis.text.x = element_text(size=10, face="bold"),
        axis.title=element_text(size=17),
        axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.95, hjust =1)) +
  scale_fill_manual(values=c("#D55E00","#0072B2")) +
  labs(x = "Location")

#plot together
BW_map <- suspension_counties_plot
ggplotly(BW_map)


#not in the app, but tells which counties' data is supressed or NA
#combining the tables (not use but can be used via grid.arrange to see the counties with NA, S, etc)
 display_table <- rbind(display_tbl_white2, display_tbl_black2)
 na_rows <- display_table %>% filter(Data == "NA")
 supr_rows <- display_table %>% filter(Data == "S")
 less_rows <- display_table %>% filter(Data == "<")
 other_rows <- display_table %>% filter(Data == "*")
 supr_rows <- mutate(supr_rows, Data = "Suppressed")
 other_rows <- mutate(other_rows, Data = "Suppressed")
 less_rows <- mutate(less_rows, Data = "less than 10")
 display_table_final <- rbind(na_rows, supr_rows, less_rows, other_rows)
 table_plot <- tableGrob(display_table_final, rows = NULL)
 
#to see the tables
#grid.arrange(suspension_counties_plot, display_table)
