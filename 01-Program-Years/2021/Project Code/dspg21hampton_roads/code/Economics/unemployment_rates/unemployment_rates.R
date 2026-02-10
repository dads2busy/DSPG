# Installs & Loads Required Packages --------------------------------------
install.packages("tidycensus", "tidyverse", "dplyr", "tigris", "ggplot2", "sf", "plotly")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(sf)
library(plotly)
library(gganimate)

# Retrieves ACS Tables ----------------------------------------------------

#identifies county fips
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

tigris_use_cache = TRUE
##hampton sum 
hamp_sum_unemp <- get_acs(geography = "county",
                          state = "VA",
                          county = county_fips,
                          year = 2019,
                          variables = c(Hampton_Roads_General_Unemployment_Rate = "S2301_C04_001"),
                          geometry = TRUE) %>% 
  mutate(estimate = (sum(estimate)) / (16)) %>%
  filter(NAME == "Gloucester County, Virginia") %>% 
  mutate(NAME = "Hampton Roads")


hamp_sum_black_unemp <- get_acs(geography = "county",
                                state = "VA",
                                county = county_fips,
                                year = 2019,
                                variables = c(Hampton_Roads_Black_Unemployment_Rate = "S2301_C04_013"),
                                geometry = TRUE) %>% 
  mutate(estimate = (sum(estimate)) / (16)) %>%
  filter(NAME == "Gloucester County, Virginia") %>% 
  mutate(NAME = "Hampton Roads")

sums <- data.frame(va_unemp_rate, va_black_unemp, hamp_sum_unemp, hamp_sum_black_unemp)


# pulls Virginia total population unemployment rate -----------------------
#2019
va_unemp_19 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2019,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

#2018
va_unemp_18 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2018,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

#2017
va_unemp_17 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2017,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

#2016
va_unemp_16 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2016,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

#2015
va_unemp_15 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2015,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

#2014
va_unemp_14 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2014,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

#2013
va_unemp_13 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2013,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

#2012
va_unemp_12 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2012,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

#2011
va_unemp_11 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2011,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

#2010
va_unemp_10 <- get_acs(geography = "state",
                         state = "VA",
                         year = 2010,
                         survey = "acs5",
                         variables = c("Virginia Unemployment Rate" = "S2301_C04_001"))

# pulls Hampton Counties total population unemployment rate ---------------

#2019
h_unemp_19 <- get_acs(geography = "county",
                              state = "VA",
                              county = county_fips,
                              year = 2019,
                              survey = "acs5",
                              variables = c("Total Population"= "S2301_C04_001"),
                              geometry = TRUE)

#2018
h_unemp_18 <- get_acs(geography = "county",
                              state = "VA",
                              county = county_fips,
                              year = 2018,
                              survey = "acs5",
                              variables = c("Total Population"= "S2301_C04_001"),
                              geometry = TRUE)

#2017
h_unemp_17 <- get_acs(geography = "county",
                              state = "VA",
                              county = county_fips,
                              year = 2017,
                              survey = "acs5",
                              variables = c("Total Population"= "S2301_C04_001"),
                              geometry = TRUE)

#2016
h_unemp_16 <- get_acs(geography = "county",
                              state = "VA",
                              county = county_fips,
                              year = 2016,
                              survey = "acs5",
                              variables = c("Total Population"= "S2301_C04_001"),
                              geometry = TRUE)

#2015
h_unemp_15 <- get_acs(geography = "county",
                              state = "VA",
                              county = county_fips,
                              year = 2015,
                              survey = "acs5",
                              variables = c("Total Population"= "S2301_C04_001"),
                              geometry = TRUE)
#2014
h_unemp_14 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2014,
                      survey = "acs5",
                      variables = c("Total Population"= "S2301_C04_001"),
                      geometry = TRUE)

#2013
h_unemp_13 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2013,
                      survey = "acs5",
                      variables = c("Total Population"= "S2301_C04_001"),
                      geometry = TRUE)

#2012
h_unemp_12 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2012,
                      survey = "acs5",
                      variables = c("Total Population"= "S2301_C04_001"),
                      geometry = TRUE)
#2011
h_unemp_11 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2011,
                      survey = "acs5",
                      variables = c("Total Population"= "S2301_C04_001"),
                      geometry = TRUE)

#2010
h_unemp_10 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2010,
                      survey = "acs5",
                      variables = c("Total Population"= "S2301_C04_001"),
                      geometry = TRUE)

# pulls Hampton Counties black population unemployment rate ---------------

#2019
b_unemp_19 <- get_acs(geography = "county",
                                state = "VA",
                                county = county_fips,
                                year = 2019,
                                survey = "acs5",
                                variables = c("Black Population" = "S2301_C04_013"),
                                geometry = TRUE)

#2018
b_unemp_18 <- get_acs(geography = "county",
                                state = "VA",
                                county = county_fips,
                                year = 2018,
                                survey = "acs5",
                                variables = c("Black Population" = "S2301_C04_013"),
                                geometry = TRUE)

#2017
b_unemp_17 <- get_acs(geography = "county",
                                state = "VA",
                                county = county_fips,
                                year = 2017,
                                survey = "acs5",
                                variables = c("Black Population" = "S2301_C04_013"),
                                geometry = TRUE)

#2016
b_unemp_16 <- get_acs(geography = "county",
                                state = "VA",
                                county = county_fips,
                                year = 2016,
                                survey = "acs5",
                                variables = c("Black Population" = "S2301_C04_013"),
                                geometry = TRUE)

#2015
b_unemp_15 <- get_acs(geography = "county",
                                state = "VA",
                                county = county_fips,
                                year = 2015,
                                survey = "acs5",
                                variables = c("Black Population" = "S2301_C04_013"),
                                geometry = TRUE)

#2014
b_unemp_14 <- get_acs(geography = "county",
                                state = "VA",
                                county = county_fips,
                                year = 2014,
                                survey = "acs5",
                                variables = c("Black Population" = "S2301_C04_011"),
                                geometry = TRUE)

#2013
b_unemp_13 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2013,
                      survey = "acs5",
                      variables = c("Black Population" = "S2301_C04_011"),
                      geometry = TRUE)

#2012
b_unemp_12 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2012,
                      survey = "acs5",
                      variables = c("Black Population" = "S2301_C04_011"),
                      geometry = TRUE)

#2011
b_unemp_11 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2011,
                      survey = "acs5",
                      variables = c("Black Population" = "S2301_C04_011"),
                      geometry = TRUE)

#2010
b_unemp_10 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2010,
                      survey = "acs5",
                      variables = c("Black Population" = "S2301_C04_011"),
                      geometry = TRUE)

# Plots Proportional Black Unemployment & Population Unemployment ---------


# writes csv files for shiny app --------------------------------

#2019
unemp_19 <- rbind(b_unemp_19, h_unemp_19)                    
write_csv(unemp_19, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2019.csv")
write_csv(va_unemp_19, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2019.csv")

#2018
unemp_18 <- rbind(b_unemp_18, h_unemp_18)
write_csv(unemp_18, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2018.csv")
write_csv(va_unemp_18, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2018.csv")

#2017
unemp_17 <- rbind(b_unemp_17, h_unemp_17)
write_csv(unemp_17, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2017.csv")
write_csv(va_unemp_17, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2017.csv")

#2016
unemp_16 <- rbind(b_unemp_16, h_unemp_16)
write_csv(unemp_16, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2016.csv")
write_csv(va_unemp_16, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2016.csv")

#2015
unemp_15 <- rbind(b_unemp_15, h_unemp_15)
write_csv(unemp_15, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2015.csv")
write_csv(va_unemp_15, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2015.csv")

#2014
unemp_14 <- rbind(b_unemp_14, h_unemp_14)
write_csv(unemp_14, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2014.csv")
write_csv(va_unemp_14, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2014.csv")

#2013
unemp_13 <- rbind(b_unemp_13, h_unemp_13)
write_csv(unemp_13, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2013.csv")
write_csv(va_unemp_13, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2013.csv")

#2012
unemp_12 <- rbind(b_unemp_12, h_unemp_12)
write_csv(unemp_12, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2012.csv")
write_csv(va_unemp_12, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2012.csv")

#2011
unemp_11 <- rbind(b_unemp_11, h_unemp_11)
write_csv(unemp_11, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2011.csv")
write_csv(va_unemp_11, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2011.csv")

#2010
unemp_10 <- rbind(b_unemp_10, h_unemp_10)
write_csv(unemp_10, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/unemployment2010.csv")
write_csv(va_unemp_10, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2301FiveYearEstimates/vaunemployment2010.csv")


#graphs side by side bar chart
# #"#D55E00","#0072B2" 
# sums_stack <- sums %>% ggplot(aes(fill = variable, y = estimate, x = NAME)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   theme_minimal() +
#   labs(title = "Unemployment Rate",
#        y = "Unemployment Rate (%)",
#        x = "Geography",
#        caption = "Source: ACS 5 Year Estimate Table S2301")
# 
# sums_stack
            
unemp_19 <- rbind(b_unemp_19, h_unemp_19)                    
  
stack_unemp_19 <- unemp_19 %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
  arrange(desc(NAME)) %>% 
  ggplot(aes(fill = variable, y = estimate, x = NAME)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_hline(yintercept = va_unemp_rate$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
  geom_hline(yintercept = hamp_sum_unemp$estimate, linetype = "dashed", color = "black", show.legend = TRUE) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(title = "",
       y = "Unemployment Rate (%)",
       x = "",
       caption = "Source: ACS 5 Year Estimate Table S2301") +
  theme(axis.text.x = element_text(angle = 40)) +
  scale_fill_manual(values = c("#D55E00", "#0072B2"))

plotly_19 <- ggplotly(stack_unemp_19)

unemp_19 <- unemp_19 %>% 
  mutate(Year = "2019")

unemp_18 <- unemp_18 %>% 
  mutate(Year = "2018")

unemp_17 <- unemp_17 %>% 
  mutate(Year = "2017")

unemp_16 <- unemp_16 %>% 
  mutate(Year = "2016")

unemp_15 <- unemp_15 %>% 
  mutate(Year = "2015")

unemp_14 <- unemp_14 %>% 
  mutate(Year = "2014")

unemp_13 <- unemp_13 %>% 
  mutate(Year = "2013")

unemp_12 <- unemp_12 %>% 
  mutate(Year = "2012")

unemp_11 <- unemp_11 %>% 
  mutate(Year = "2011")

unemp_10 <- unemp_10 %>% 
  mutate(Year = "2010")

unemp <- rbind(unemp_19, unemp_18, unemp_17, unemp_16, unemp_15, unemp_14, unemp_13, unemp_12, unemp_11, unemp_10)

unemp <- unemp %>% 
  select(NAME, variable, estimate, Year) %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) 

unemp_plot <- unemp %>% 
  ggplot(aes(fill = variable, y = estimate, x = NAME)) +
  geom_bar(position = "dodge", stat = "identity") +
  transition_states(Year, transition_length = 4, state_length = 1)
  
animate(unemp_plot, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("unemp_plot.gif"))



