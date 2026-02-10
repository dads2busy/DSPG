#Load required Packages
#setwd("C:/Users/dgthomas/Documents")
library(DSPG)
# packages <- c("plyr", "dplyr", "purrr", "readr", "stringr", "readr", "readxl",
#               "magrittr", "stringr", "ggplot2", "shiny", "sentimentr",
#               "shinythemes", "shinydashboard", "shinydashboardPlus", "plotly", "prophet","data.table", "dygraphs","ggthemes","lubridate","devtools","sf","leaflet","tidyr","tibble", "mapview","plainview","viridis","hrbrthemes")
# 
# for (pkgs in packages){
#   if(!require(pkgs, character.only = TRUE)){ # Condition
#     install.packages(pkgs) # Install if not
#     library(pkgs) # Load if installed
#   }
# }
library("dplyr")	
library("purrr")	
library("readr")	
library("stringr")	
library("readr")	
library("readxl")	
library("magrittr")	
library("stringr")	
library("ggplot2")	
library("shiny")	
#library("sentimentr",	
library("shinythemes")	
library("shinydashboard")	
library("shinydashboardPlus")	
library("plotly")	
library("prophet")	
library("data.table")	
library("dygraphs")	
library("ggthemes")	
library("lubridate")	
library("devtools")	
library("sf")	
library("leaflet")	
library("tidyr")	
library("tibble")
library("mapview")	
library("plainview")
library("viridis")
library("hrbrthemes")

liquor_sales <- read_csv("data/Iowa_Liquor_Sales_by_Year_and_County.csv") #Named df_1 originally
# clean data set
liquor_sales <- liquor_sales %>% separate(Date, into = c("month","day","year", "time"))
liquor_sales <- as.data.frame(liquor_sales)
#Tidy datasets
liquor_sales$County <- tolower(liquor_sales$County)
liquor_sales$year <- as.factor(liquor_sales$year )
liquor_sales[liquor_sales == "o'brien"] <- "obrien"
liquor_sales[liquor_sales == "cerro gord"] <- "cerro gordo"
liquor_sales[liquor_sales == "buena vist"] <- "buena vista"
liquor_sales[liquor_sales == "pottawatta"] <- "pottawattamie"

###Changed Xinyi's Code###
liquor_sales_grouped <- liquor_sales %>%
  group_by(County, year) %>%
  dplyr::summarise(sumVolumeSold = sum(`Volume Sold (Gallons)`))

#join alcohol sales data set with census data by county
pop_trends <- read.csv("data/IA_county_PopulationTotalTrends.csv") #Named ppl originally

pop_trends_selected<- pop_trends %>%
  select(county, e2012:e2019)
pop_trends_selected$county <-tolower(pop_trends_selected$county)
pop_trends_selected[pop_trends_selected == "o'brien"] <- "obrien"
# rename
head(pop_trends_selected)
colnames(pop_trends_selected) <- c("County",2012, 2013, 2014, 2015,2016,2017,2018,2019)
pop_trends_tibble <- pop_trends_selected %>%
  pivot_longer("2012":"2019", names_to = 'year', values_to='population') #Originally called ppl_2
pop_trends_tibble[pop_trends_tibble == "o'brien"] <- "obrien"
#Merge data sets
liquor_sales_by_pop <- merge(liquor_sales_grouped, pop_trends_tibble,by=c("County", "year")) ##Initially called total
colnames(liquor_sales_by_pop)[1] <-"COUNTY"
#Plotting Map
mymap<-st_read("data/county.shp")%>% st_transform(crs = 4326)
#mymap<-ia_counties
str(mymap)

mymap$COUNTY <- tolower(mymap$COUNTY)
mapData <- merge(mymap,liquor_sales_by_pop,by="COUNTY")
#view(mapData)
#Commenting because I am going to use reactivity
# mapData = mapData %>% 
#   filter(year ==2018)

# ggplot(mapData) +
#   geom_sf(aes(fill=sumVolumeSold/population)) + 
#   geom_sf_text(aes(label=COUNTY), colour = "white", size=2.5)+
#   ggtitle("Liquor Sales per county per capita in 2018")


#Loading Iowa Unemployment Data
Iowa_unemployment<-read.csv("data/Iowa LAUS Data 2020-05.csv")
#Selecting the last 10 years duration
Iowa_unemployment_data <- Iowa_unemployment %>% filter(YEAR >= 2010)
#Checking if it's the 10 years wanted
unique(Iowa_unemployment_data$YEAR) 
#Only include county
Iowa_unemployment_data<- Iowa_unemployment_data %>% filter(AREATYNAME == "County")
#Mean for unemployment per year per county

Iowa_unemployment_data_selected <- Iowa_unemployment_data %>%
  select(AREANAME, YEAR, UNEMP,UNEMPRATE,MONTH) ##Initially called ddf

##Changed Xinyi's Code###
# Iowa_unemployment_data_selected_grouped <- Iowa_unemployment_data_selected %>%
#   group_by(AREANAME, YEAR) %>%
#   mutate(mean_rate = mean(UNEMPRATE))%>% ##Initially called ddf_1
#   summarise(mean_ratePerYear = sum(mean_rate)/12)


##Xinyi and Kent will be changing this later
Iowa_unemployment_data_selected_Jan = Iowa_unemployment_data_selected%>% filter(MONTH == 'January')
Iowa_unemployment_data_selected_grouped <- Iowa_unemployment_data_selected_Jan %>%
  group_by(AREANAME, YEAR) %>%
  dplyr::summarise(mean_ratePerYear = UNEMPRATE) ##Initially called ddf_1
#summarise(mean_ratePerYear = sum(mean_rate)/12)
#mymap_unemployment<-st_read("county.shp")%>% st_transform(crs = 4326) ##It was mymap before

Iowa_unemployment_data_selected_grouped_mod<- Iowa_unemployment_data_selected_grouped %>% mutate(COUNTY = str_remove_all(AREANAME, " County")) ##It was ddf_1
Iowa_unemployment_data_final_sel <- Iowa_unemployment_data_selected_grouped_mod[, c(4,2,3)] ##It was ddf_2

Iowa_unemployment_data_final_sel$COUNTY <-tolower(Iowa_unemployment_data_final_sel$COUNTY)
Iowa_unemployment_data_final_sel[Iowa_unemployment_data_final_sel == "o'brien"] <- "obrien"
#mymap_unemployment$COUNTY <- tolower(mymap_unemployment$COUNTY)
Iowa_unemployment_data_final_sel = dplyr::rename(Iowa_unemployment_data_final_sel, year = YEAR)
#map_Data_Unemp <- merge(mymap_unemployment,Iowa_unemployment_data_final_sel,by="COUNTY")
#Merging 2 maps together
Iowa_unemployment_data_final_sel$year = as.factor(Iowa_unemployment_data_final_sel$year)
map_Data_Unemp_sales <- merge(mapData,Iowa_unemployment_data_final_sel,by=c("COUNTY","year"))
#New column having volume sold per capita
map_Data_Unemp_sales$Vol_solpercap = with(map_Data_Unemp_sales, sumVolumeSold/population)
map_Data_Unemp_sales$year = as.factor(map_Data_Unemp_sales$year)
###Commenting because I am going to use reactivity
# map_Data_Unemp = map_Data_Unemp %>% 
#   filter(YEAR ==2018)
#check number of counties data
#Changed Xinyi's Code
c<-count(map_Data_Unemp_sales, vars = "COUNTY") %>% head(99)
c

# x = map_Data_Unemp$sumVolumeSold/map_Data_Unemp$population
# y = map_Data_Unemp$mean_ratePerYear
bins <- c(0,1,2,3,Inf)

bins_1 <- c(min(map_Data_Unemp_sales$mean_ratePerYear), (mean(map_Data_Unemp_sales$mean_ratePerYear)-1*sd(map_Data_Unemp_sales$mean_ratePerYear)), (mean(map_Data_Unemp_sales$mean_ratePerYear)),(mean(map_Data_Unemp_sales$mean_ratePerYear)+1*sd(map_Data_Unemp_sales$mean_ratePerYear)),max(map_Data_Unemp_sales$mean_ratePerYear))

pal <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = map_Data_Unemp_sales$Vol_solpercap, bins = 5, pretty = FALSE)
pal_1 <- colorBin(c("#badcf7", "#3A7AB2", "#2E6392", "#255075", "#012340"), domain = map_Data_Unemp_sales$mean_ratePerYear, bins = 5, pretty = FALSE)
###########################Deepak's Portion#####################################
# Commented out so that it can thrown into Github
# df = fread('Iowa_Liquor_Sales.csv',select =c("Date","Volume Sold (Gallons)"))
# df = dplyr::rename(df, Volume = `Volume Sold (Gallons)`)
# df1 = aggregate(df$Volume, by=list(Date=df$Date), FUN=sum)
# df1 = dplyr::rename(df1, ds = Date, y = x)
# df1$ds <- as.Date(df1$ds , format = "%m/%d/%Y")
# df1 = transform(df1, day = wday(ds))
# df1<-df1[!(df1$day==6),]
# df1<-df1[!(df1$day==7),]
# df1<-df1[!(df1$day==1),]
# write.csv(df1,"Iowa_Liquor_Sales_Agg.csv")

Iowa_Liquor_Sales_Agg = read.csv('data/Iowa_Liquor_Sales_Agg.csv')
Iowa_Liquor_Sales_model <- prophet(Iowa_Liquor_Sales_Agg)
Iowa_Liquor_Sales_future <- make_future_dataframe(Iowa_Liquor_Sales_model, periods = 365)
tail(Iowa_Liquor_Sales_future)
Iowa_Liquor_Sales_forecast <- predict(Iowa_Liquor_Sales_model, Iowa_Liquor_Sales_future)
tail(Iowa_Liquor_Sales_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


# Works but commented so that data can be thrown into Github
# CollegeTowns_Sales_18 = fread('2016-2018Iowa_Liquor_Sales(CollegeTowns).csv',select =c("Date","Volume Sold (Gallons)"))
# CollegeTowns_Sales_19 = fread('2019Iowa_Liquor_Sales(CollegeTowns).csv',select =c("Date","Volume Sold (Gallons)"))
# CollegeTowns_Sales = rbind(CollegeTowns_Sales_18,CollegeTowns_Sales_19)
# CollegeTowns_Sales = dplyr::rename(CollegeTowns_Sales, Volume = 'Volume Sold (Gallons)')
# CollegeTowns_Sales = aggregate(CollegeTowns_Sales$Volume, by=list(Date=CollegeTowns_Sales$Date), FUN=sum)
# CollegeTowns_Sales = dplyr::rename(CollegeTowns_Sales, ds = Date, y = x)
# CollegeTowns_Sales$ds <- as.Date(CollegeTowns_Sales$ds , format = "%m/%d/%Y")
# CollegeTowns_Sales = transform(CollegeTowns_Sales, day = wday(ds))
# CollegeTowns_Sales$cap = 8000
#write.csv(CollegeTowns_Sales,"Campus_Town_Agg.csv")

Campus_Town_Agg = read.csv('data/Campus_Town_Agg.csv')
Campus_Town_Agg_model <- prophet(Campus_Town_Agg,growth = 'logistic')
Campus_Town_Agg_future <- make_future_dataframe(Campus_Town_Agg_model, periods = 365)
Campus_Town_Agg_future$cap = 8000
tail(Campus_Town_Agg_future)
Campus_Town_Agg_forecast <- predict(Campus_Town_Agg_model, Campus_Town_Agg_future)
tail(Campus_Town_Agg_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])



#This runs but commented out so it can be thrown into Github
# crash_data = read.csv(file = 'crash data june 19 2020 (1).csv')
# crash_data_filtered = filter(crash_data, Drug.or.Alcohol.Related == 'Alcohol (Statutory)'|Drug.or.Alcohol.Related == 'Under Influence of Alcohol/Drugs/Medications'|Drug.or.Alcohol.Related == 'Drug/Alcohol (Statutory)')
# crash_data_filtered = filter(crash_data_filtered, Driver.Condition == 'Under the influence of alcohol')
# crash_data_dates = crash_data_filtered %>%select(Date.of.Crash)
# crash_data_dates = crash_data_dates %>% dplyr::count(Date.of.Crash)
# crash_data_dates = dplyr::rename(crash_data_dates, ds = Date.of.Crash, y = n)
# crash_data_dates$ds <- as.Date(crash_data_dates$ds , format = "%Y%m%d")
# crash_data_dates = transform(crash_data_dates, day = wday(ds))
# write.csv(crash_data_dates,"Crashes.csv")

Crashes = read_csv('data/Crashes.csv')
Crashes_model = prophet(Crashes)
Crashes_future = make_future_dataframe(Crashes_model, periods = 365)
tail(Crashes_future)
Crashes_forecast = predict(Crashes_model,Crashes_future)
tail(Crashes_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


city_lvl_liquor_sales = fread("cities_Liquor_Sales.csv", select = c("city", "longitude",	"latitude",	"state",	"pc_g2019", "pc_g2018", "pc_g2017", "pc_g2016", "pc_g2015", "pc_g2014", "pc_g2013", "pc_g2012"))
# colnames(city_lvl_liquor_sales) <- c("city", "longitude",	"latitude",	"state", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012")
# city_lvl_liquor_sales_tbbl <- city_lvl_liquor_sales # %>%
# pivot_longer("2012":"2019", names_to = 'year', values_to='sales')
#bin_sales = c(min(city_lvl_liquor_sales_tbbl$sales, na.rm = T), 1000,(mean(city_lvl_liquor_sales_tbbl$sales, na.rm = T)),(mean(city_lvl_liquor_sales_tbbl$sales,na.rm = T)+1*sd(city_lvl_liquor_sales_tbbl$sales, na.rm = T)),max(city_lvl_liquor_sales_tbbl$sales, na.rm = T))
#pal_sales <- colorBin(c("red","blue","green","orange"), domain = city_lvl_liquor_sales_tbbl$sales, bins = bin_sales)

brfss = fread("BRFSS.csv")
hvydrnks = brfss$hvydrinker
x= brfss %>% filter(hvydrinker %in% c("Not a heavy drinker","Heavy drinker"))
geom_bar(x= x)

index<-read_excel("BroaderIndex_Demo.xlsx")
newmap=st_read("data/county.shp")%>% st_transform(crs = 4326)
newmap$COUNTY = tolower(newmap$COUNTY)
index$COUNTY = tolower(index$COUNTY)
indexmap <-merge(mymap, index,by="COUNTY")



#s = leafsync::sync(m1, m2, m3, m4,m5,m6,m7)
#s

#read data
newmap_xinyi=st_read("data/county.shp")%>% st_transform(crs = 4326)

index_map <- read_excel("alcohol_Index.xlsx")
index_SFD <- read_csv("alcohol_Index_SFD.csv")
colnames(index_SFD)[3] <-"COUNTY"
index_SFD[index_SFD == "o'brien"] <- "obrien"
index_SFD <- index_SFD[1:99,]


#Join Data
colnames(index_map)[3] <-"COUNTY"
index_map$COUNTY <-tolower(index_map$COUNTY)
newmap_xinyi$COUNTY <-tolower(newmap_xinyi$COUNTY)
index_map[index_map == "O'Brien"] <- "Obrien"
index_map_final <-merge(newmap_xinyi,index_map,by="COUNTY")

index_SFD$COUNTY <-tolower(index_SFD$COUNTY)
index_SFD_final <-merge(newmap_xinyi,index_SFD,by="COUNTY")
#s1 = leafsync::sync(m8, m9, m10, m11, m12)
#s1

# read city level data set
cityLevel <- 
  readxl::read_xlsx("cities_Liquor_Sales.xlsx")

# tidy the frame and make YEAR as a variable
cityLevel_1 <- cityLevel %>%
  pivot_longer("e2019" :"e2010",names_to = 'year', values_to='liquorSales' ) %>%
  pivot_longer("pc_g2019" :"pc_g2012",names_to = 'Year', values_to='liquorSalesPerCapita')
summary(cityLevel_1)
str(cityLevel_1)
min(cityLevel_1$liquorSalesPerCapita)

cityLevel_1$liquorSalesPerCapita[cityLevel_1$liquorSalesPerCapita == 0] <- NA


# interactive line plot (e2012 to e2019)
cityLevel_1 %>%
  plot_ly(x = ~year, 
          y = ~liquorSales, name = 'city', type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "Liquor Sales for City",
         xaxis = list(title = "Year"),
         yaxis = list (title = "Liquor Sales (gallon)"))



cl<- cityLevel %>%
  select(city,"pc_g2019" :"pc_g2012") %>%
  pivot_longer("pc_g2019" :"pc_g2012",names_to = 'Year', values_to='liquorSalesPerCapita')

cl_1<- cl %>% mutate(Year = str_remove_all(Year, "pc_g"))

a <-cl_1 %>%
  ggplot(aes(x=Year, y=liquorSalesPerCapita, group=city)) +
  geom_line(aes(color = city))+
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  ggtitle("")




save.image(file = "demos.RData")
