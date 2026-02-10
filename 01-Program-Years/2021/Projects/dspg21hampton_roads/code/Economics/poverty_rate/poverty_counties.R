library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)

#hampton roads data change the year in the function and filename to get the other years
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
#generating excels
write.csv(hamp_pov, file = "shinyapp/data/TableS1701FiveYearEstimates/hamp_poverty2019.csv")

#2015 - 2019 format
hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2015.csv")

hamp_pov <- hamp_pov[,2:6]
hamp_pov_tbl <- hamp_pov %>%
  group_by(NAME) %>%
  slice(123)
hamp_pov_blck_tbl <- hamp_pov %>%
  group_by(NAME) %>%
  slice(136)
hamp_pov_tbl %>% ungroup()
hamp_pov_blck_tbl %>% ungroup()
hamp_pctG <- hamp_pov_tbl[,4]
hamp_pctB <- hamp_pov_blck_tbl[,4] 
hamp_comb <- rbind(hamp_pctG, hamp_pctB)
colnames(hamp_comb) <- "Ratio"

##change string and filter line
hamp_comb <- mutate(hamp_comb, Location =  rep(c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                 "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                 "Suffolk", "Virginia Beach", "Williamsburg", "York"),2))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
#taking out poquoson
hamp_comb <- hamp_comb %>% filter(hamp_comb[,2] != "Poquoson")

#Graph 
counties_pov <-  ggplot(hamp_comb, aes(Location, y=`Percentage (%)`, fill=Demographic)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
            position = position_dodge(0.9), size=3)+
  theme_minimal() +
  scale_fill_manual(values=c("#D55E00","#0072B2")) +
  theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(0., 'cm'), 
        legend.key.width = unit(0.5, 'cm'), 
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.text=element_text(size=15),
        #axis.text.x = element_text(size=8, face="bold"),
        axis.title=element_text(size=17),
        axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(angle=40, vjust=0.95, hjust=1))

#plot
counties_pov 


#2012 - 2014 format
hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2012.csv")

hamp_pov <- hamp_pov[,2:6]
hamp_pov_tbl <- hamp_pov %>%
  group_by(NAME) %>%
  slice(93)
hamp_pov_blck_tbl <- hamp_pov %>%
  group_by(NAME) %>%
  slice(102)
hamp_pov_tbl %>% ungroup()
hamp_pov_blck_tbl %>% ungroup()
hamp_pctG <- hamp_pov_tbl[,4]
hamp_pctB <- hamp_pov_blck_tbl[,4] 
hamp_comb <- rbind(hamp_pctG, hamp_pctB)
colnames(hamp_comb) <- "Ratio"

#labeling
hamp_comb <- mutate(hamp_comb, Location = c(rep(c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                  "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                  "Suffolk", "Virginia Beach", "Williamsburg", "York"),2)))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
#taking out poquoson
hamp_comb <- hamp_comb %>% filter(hamp_comb[,2] != "Poquoson")

#Graph 
ggplot(hamp_comb, aes(Location, y=`Percentage (%)`, fill=Demographic)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
            position = position_dodge(0.9), size=3)+
  theme_minimal() +
  scale_fill_manual(values=c("#D55E00","#0072B2")) +
  theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        axis.text=element_text(size=15),
        #axis.text.x = element_text(size=10, face="bold"),
        axis.title=element_text(size=17),
        axis.title.x=element_blank())+
  theme(axis.text.x = element_text(angle=40, vjust=0.95, hjust=1))
  
  #plot
  counties_pov
