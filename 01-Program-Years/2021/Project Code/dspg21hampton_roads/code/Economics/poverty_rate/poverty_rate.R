library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)

#get data for 2010-2019 by changing the year in the function and the filename
#va data
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}

#generating excels
va_pov <- va_table("S1701", 2012)
write.csv(va_pov, file = "shinyapp/data/TableS1701FiveYearEstimates/va_poverty2012.csv")

#hampton data
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

hamp_pov <- get_acs(geography = "county", state = 51,
                    county = county_fips[1],
                    table = "S1701",
                    year = 2012)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "S1701",
                 year = 2012)
  hamp_pov <- rbind(hamp_pov, tmp)
}
#generating excels
write.csv(hamp_pov, file = "shiny/data/TableS1701FiveYearEstimates/hamp_poverty2012.csv")


#2015-2019 format
#VA
va_pov <- read.csv("shinyapp/data/TableS1701FiveYearEstimates/va_poverty2019.csv")
va_pov <- va_pov[,2:6]
#General
va_pct_pov <- va_pov[123,4]
#Black
va_pct_pov_blck <- va_pov[136,4]
va_pov_vector <- rbind(va_pct_pov, va_pct_pov_blck)

#Hampton
#read in csv
hamp_pov <- read.csv("shinyapp/data/TableS1701FiveYearEstimates/hamp_poverty2019.csv")
hamp_pov <- hamp_pov[,2:6]

#General
hamp_total <- hamp_pov %>%
  group_by(NAME) %>%
  slice(c(1))
hamp_total2 <- colSums(hamp_total[,4])

hamp_pov2 <- hamp_pov %>%
  group_by(NAME) %>%
  slice(c(62))
hamp_pov3 <- colSums(hamp_pov2[,4])

hamp_overall_pov <- hamp_pov3/hamp_total2 *100

#Black
hamp_total_blck <- hamp_pov %>%
  group_by(NAME) %>%
  slice(14)
hamp_total_blck2 <- colSums(hamp_total_blck[,4]) 

hamp_pov_blck <- hamp_pov %>%
  group_by(NAME) %>%
  slice(75)
hamp_pov_blck2 <- colSums(hamp_pov_blck[,4])
hamp_blck_overall_pov <- hamp_pov_blck2/hamp_total_blck2*100
hamp_pov_vector <- rbind(hamp_overall_pov, hamp_blck_overall_pov)

#combing hampton and VA
pov_pct <- data.frame(va_pov_vector, hamp_pov_vector)
pov_pct2 <- data.frame(Ratio = unlist(pov_pct, use.names=FALSE))

#Graphing hampton and VA
pov_pct2 <- mutate(pov_pct2, Location = c("Virginia", "Virginia", "Hampton Roads", "Hampton Roads"))
pov_pct2 <- mutate(pov_pct2, Demographic = c("Total Population", "Black Population", "Total Population",
                                             "Black Population"))
colnames(pov_pct2) <- c("Percentage (%)", "Location", "Demographic")
pov_pct2$Location <- factor(pov_pct2$Location, levels = c("Hampton Roads", "Virginia"))

#Graph for just hampton roads and VA
pov_plot <- ggplot(pov_pct2, aes(x=Location, y=`Percentage (%)`, fill=Demographic)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
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
  scale_fill_manual(values=c("#D55E00","#0072B2")) +
  ylim(0, 20) 


#2012-2014 format
#VA
va_pov <- read.csv("shinyapp/data/TableS1701FiveYearEstimates/va_poverty2012.csv")
va_pov <- va_pov[,2:6]
#General
va_pct_pov <- va_pov[93,4]
#Black
va_pct_pov_blck <- va_pov[102,4]
va_pov_vector <- rbind(va_pct_pov, va_pct_pov_blck)
#Hampton Roads
hamp_pov <- read.csv("shinyapp/data/TableS1701FiveYearEstimates/hamp_poverty2012.csv")
hamp_pov <- hamp_pov[,2:6]
#General
hamp_total <- hamp_pov %>%
  group_by(NAME) %>%
  slice(c(1))
hamp_total2 <- colSums(hamp_total[,4])
hamp_pov2 <- hamp_pov %>%
  group_by(NAME) %>%
  slice(c(47))
hamp_pov3 <- colSums(hamp_pov2[,4])
hamp_overall_pov <- hamp_pov3/hamp_total2 *100
#Black
hamp_total_blck <- hamp_pov %>%
  group_by(NAME) %>%
  slice(10)
hamp_total_blck2 <- colSums(hamp_total_blck[,4]) 
hamp_pov_blck <- hamp_pov %>%
  group_by(NAME) %>%
  slice(56)
hamp_pov_blck2 <- colSums(hamp_pov_blck[,4])
hamp_blck_overall_pov <- hamp_pov_blck2/hamp_total_blck2*100
hamp_pov_vector <- rbind(hamp_overall_pov, hamp_blck_overall_pov)
#combing hampton and VA
pov_pct <- data.frame(va_pov_vector, hamp_pov_vector)
pov_pct2 <- data.frame(Ratio = unlist(pov_pct, use.names=FALSE))
pov_pct2 <- mutate(pov_pct2, Location = c(rep("Virginia",2), rep("Hampton Roads",2)))
pov_pct2 <- mutate(pov_pct2, Demographic = rep(c("Total Population", "Black Population"),2))
colnames(pov_pct2) <- c("Percentage (%)", "Location", "Demographic")
pov_pct2$Location <- factor(pov_pct2$Location, levels = c("Hampton Roads", "Virginia"))
#Graph for just hampton roads and VA
pov_plot <- ggplot(pov_pct2, aes(x=Location, y=`Percentage (%)`, fill=Demographic)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
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
  scale_fill_manual(values=c("#D55E00","#0072B2"))
