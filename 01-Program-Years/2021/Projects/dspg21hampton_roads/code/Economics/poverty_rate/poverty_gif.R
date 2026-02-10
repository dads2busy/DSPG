library(dplyr)
library(ggplot2)
library(gganimate)
#to update the gif add a section (code between the hash tags from a year between 2015-2019) and change the year to 2020 then add pov20 to poverty_counties

#getting hampton data from 2012-2019 (change the year in the function and in the file name)
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


#2019
hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2019.csv")
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
hamp_comb <- mutate(hamp_comb, Location =  rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                 "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                 "Suffolk", "Virginia Beach", "Williamsburg", "York"),2))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
pov19 <- mutate(hamp_comb, year = "2019")
###########################################################################
#2018
hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2018.csv")
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
hamp_comb <- mutate(hamp_comb, Location =  rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                 "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                 "Suffolk", "Virginia Beach", "Williamsburg", "York"),2))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
pov18 <- mutate(hamp_comb, year = "2018")
############################################################################
#2017
hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2017.csv")
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
hamp_comb <- mutate(hamp_comb, Location =  rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                 "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                 "Suffolk", "Virginia Beach", "Williamsburg", "York"),2))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
pov17 <- mutate(hamp_comb, year = "2017")
##############################################################################
#2016
hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2016.csv")
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
hamp_comb <- mutate(hamp_comb, Location =  rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                 "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                 "Suffolk", "Virginia Beach", "Williamsburg", "York"),2))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
pov16 <- mutate(hamp_comb, year = "2016")
##############################################################################
#2015
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
hamp_comb <- mutate(hamp_comb, Location =  rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                 "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                 "Suffolk", "Virginia Beach", "Williamsburg", "York"),2))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
pov15 <- mutate(hamp_comb, year = "2015")
#############################################################################new format
#2014
hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2014.csv")
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
hamp_comb <- mutate(hamp_comb, Location = c(rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                  "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                  "Suffolk", "Virginia Beach", "Williamsburg", "York"),2)))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
pov14 <- mutate(hamp_comb, year = "2014")
###############################################################################
#2013
hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2013.csv")
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
hamp_comb <- mutate(hamp_comb, Location = c(rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                  "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                  "Suffolk", "Virginia Beach", "Williamsburg", "York"),2)))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
pov13 <- mutate(hamp_comb, year = "2013")
###############################################################################
#2012
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
hamp_comb <- mutate(hamp_comb, Location = c(rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                  "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                  "Suffolk", "Virginia Beach", "Williamsburg", "York"),2)))
hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                               rep("Black Population",16)))
colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
pov12 <- mutate(hamp_comb, year = "2012")
######################################################################combining
poverty_counties <- rbind(pov12, pov13, pov14, pov15, pov16, pov17, pov18)
#makes a csv with all the years' data
write.csv(poverty_counties, file = "data/TableS1701FiveYearEstimates/hamp_povertyGif.csv")


#read in csv
poverty_counties <- read.csv("data/TableS1701FiveYearEstimates/hamp_povertyGif.csv")
poverty_counties$Location[poverty_counties$Location == "Franklin"] <- "Franklin City"
poverty_counties2 <- poverty_counties %>% filter(poverty_counties[,3] != "Poquoson")

#graph
poverty_static <- ggplot(poverty_counties2, aes(y=poverty_counties2[,2], x=Location, fill=Demographic)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=paste0(round(poverty_counties2[,2], digits=2))), vjust=2, color="white",
            position = position_dodge(0.9), size=6) +
  scale_fill_manual(values=c("#D55E00","#0072B2"), labels= c("Black Population", "Total Population")) +
  ylab("Percentage (%)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size =20),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        axis.text = element_text(size=20),
        axis.text.x = element_text(angle=40, vjust=0.95, hjust=1)) 

animated_poverty_counties <- poverty_static + transition_states(year, transition_length=4, state_length = 1) +
  labs(title = "Poverty Rates: {closest_state}") + theme(plot.title = element_text(size = 20, hjust = 0.5))


#creates the actual gif
animate(animated_poverty_counties, 200, fps=20, width =1500, height =1000, renderer =gifski_renderer("poverty.gif"))

