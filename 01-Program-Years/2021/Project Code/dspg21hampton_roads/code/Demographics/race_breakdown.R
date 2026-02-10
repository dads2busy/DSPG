library(Rcpp)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(scales)
library(gridExtra)
library(grid)
library(Rcpp)
library(forcats)
#to update change the year in the two functions (va_table and the hamp_data loop)

##Race for VA
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}
races1 <- va_table("B02001", 2010)

#convert to csv
write.csv(races, file = "shinyapp/data/TableB02001FiveYearEstimates/va_race2010.csv")
#read in csv
races <- read.csv("shinyapp/data/TableB02001FiveYearEstimates/va_race2010.csv")
races <- races[,2:6]
total <- races[1,4]
#Now graphing with other = hawaiin/pi, america/alask naive. other
va_races <-  data.frame(t((races[c(2:8), 4])))
va_races <- mutate(va_races, X8 = X3+X5+X6)
va_races2 <- data.frame(t(va_races[,c(1,2,4,7,8)]))
colnames(va_races2) <- "estimate"
va_races2 <- mutate(va_races2, totl = total)
va_races2 <- mutate(va_races2, pct = estimate/totl*100)
va_races2 <- mutate(va_races2, race = c("White", "Black", "Asian", "Two or more", "Other"))

#color blind friendly palette
vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")

#adds a column for the positioning of the labels
va_races3 <- va_races2 %>% 
  mutate(
    cs = rev(cumsum(rev(pct))), 
    pos = pct/2 + lead(cs, 1),
    pos = if_else(is.na(pos), pct/2, pos))

#graph
va_pie <- ggplot(va_races3, aes(x = "" , y = pct, fill = fct_inorder(race))) +
  geom_col(width = 1) +
  coord_polar(theta = "y", start = 0 ) +
  geom_label_repel(aes(y = pos, label = paste0(round(pct, digits=2), "%")),
                   data = va_races3, size=4, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Key")) +
  theme_void() +ggtitle("Virginia") +
  theme(plot.title = element_text(hjust = 0.5, size=25, vjust =5),
        legend.title = element_blank()) +
  scale_fill_manual(values =vir_pal)


##Race for Hampton Roads
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

hamp_data <- get_acs(geography = "county", state = 51,
                     county = county_fips[1],
                     table = "B02001",
                     year = 2010)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "B02001",
                 year = 2010)
  hamp_data <- rbind(hamp_data, tmp)
}

#convert to csv
write.csv(hamp_data, file = "shinyapp/data/TableB02001FiveYearEstimates/hamp_race2010.csv")
#read in csv
hamp_data <- read.csv("shinyapp/data/TableB02001FiveYearEstimates/hamp_race2010.csv")
hamp_data <- hamp_data[,2:6]

#combining data from counties
variable <- sample(c("B02001_001", "B02001_002","B02001_003", "B02001_004",
                     "B02001_005", "B02001_006", "B02001_007", "B02001_008",
                     "B02001_009", "B02001_010"),160, replace = TRUE)

hamp_data <- hamp_data %>% group_by(variable) %>% summarize(sum(estimate))
#select the column and rows we want
hamp_races <- hamp_data[c(2:8),]

#Now graphing with other = hawaiin/pi, america/alask naive. other
hamp_races3 <- data.frame(t(hamp_data[c(2:8),2]))
hamp_races3 <- mutate(hamp_races3, X8 = X3+X5+X6)
hamp_races4 <- data.frame(t(hamp_races3[,c(1,2,4,7,8)]))
colnames(hamp_races4) <- "estimate"
total_pop = sum(hamp_races4$estimate)
hamp_races4 <- mutate(hamp_races4, total = total_pop)
hamp_races4 <- mutate(hamp_races4, pct = estimate/total*100)
hamp_races4 <- mutate(hamp_races4, race = c("White", "Black", "Asian", "Two or more", "Other"))
colnames(hamp_races4) <- c("estimate", "Total", "Percent of Population", "race")

#color blind friendly palette
vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")

#adds a column for the positioning of the labels
hamp_races5 <- hamp_races4 %>% 
  mutate(
    cs = rev(cumsum(rev(`Percent of Population`))), 
    pos = `Percent of Population`/2 + lead(cs, 1),
    pos = if_else(is.na(pos),`Percent of Population`/2, pos))

#graph
hamp_pie <- ggplot(hamp_races5, aes(x = "" , y = `Percent of Population`, fill = fct_inorder(race))) +
  geom_col(width = 1) +
  coord_polar(theta = "y", start = 0 ) +
  geom_label_repel(aes(y = pos, label = paste0(round(`Percent of Population`, digits=2), "%")),
                   data = hamp_races5, size=4, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Key")) +
  theme_void() +ggtitle("Hampton Roads") +
  theme(plot.title = element_text(hjust = 0.5, size=25, vjust =5),
        legend.title = element_blank()) +
  scale_fill_manual(values =vir_pal)

#outputs
hamp_pie
va_pie

