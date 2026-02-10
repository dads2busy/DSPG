# Installs & Loads Required Packages --------------------------------------
install.packages("tidycensus", "tidyverse", "dplyr", "tigris", "ggplot2", "sf")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(sf)
library(plotly)

# Retrieves ACS Tables ----------------------------------------------------
#identifies county fips
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

tigris_use_cache = TRUE

#pulls total uninsured Virginia population and calculates percentage
va_unins_19 <- get_acs(geography = "state",
                  state = "VA",
                  year = 2019,
                  survey = "acs5",
                  variables = c("VA Population" = "S2701_C04_001"),
                  summary_var = "S2701_C01_001") %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)
  

#pulls total uninsured Hampton population and calculates percentage
hampton_overall <- get_acs(geography = "county",
                           state = "VA",
                           county = county_fips,
                           year = 2019,
                           variables = "S2701_C04_017",
                           summary_var = "S2701_C01_017") %>% 
  mutate(estimate = 100 * (sum(estimate) / sum(summary_est)))

# pulls general population uninsured percentages --------------------------

#2019
hamp_unins_19 <- get_acs(geography = "county",
                           state = "VA",
                           county = county_fips,
                           year = 2019,
                           survey = "acs5",
                           variables = c("Total Population" = "S2701_C04_001"),
                           summary_var = "S2701_C01_001",
                           geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

#2018
hamp_unins_18 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         year = 2018,
                         survey = "acs5",
                         variables = c("Total Population" = "S2701_C04_001"),
                         summary_var = "S2701_C01_001",
                         geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

#2017
hamp_unins_17 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         year = 2017,
                         survey = "acs5",
                         variables = c("Total Population" = "S2701_C04_001"),
                         summary_var = "S2701_C01_001",
                         geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

#2016
hamp_unins_16 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         year = 2016,
                         survey = "acs5",
                         variables = c("Total Population" = "S2701_C04_001"),
                         summary_var = "S2701_C01_001",
                         geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

#2015
hamp_unins_15 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         year = 2015,
                         survey = "acs5",
                         variables = c("Total Population" = "S2701_C04_001"),
                         summary_var = "S2701_C01_001",
                         geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

#2014
hamp_unins_14 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         year = 2014,
                         survey = "acs5",
                         variables = c("Total Population" = "S2701_C02_001"),
                         summary_var = "S2701_C01_001",
                         geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

#2013
hamp_unins_13 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         year = 2013,
                         survey = "acs5",
                         variables = c("Total Population" = "S2701_C02_001"),
                         summary_var = "S2701_C01_001",
                         geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

#2012
hamp_unins_12 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         year = 2012,
                         survey = "acs5",
                         variables = c("Total Population" = "S2701_C02_001"),
                         summary_var = "S2701_C01_001",
                         geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

#2011
hamp_unins_11 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         year = 2011,
                         survey = "acs1",
                         variables = c("Total Population" = "S2701_C02_001"),
                         summary_var = "S2701_C01_001",
                         geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

#2010
hamp_unins_10 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         year = 2010,
                         survey = "acs1",
                         variables = c("Total Population" = "S2701_C02_001"),
                         summary_var = "S2701_C01_001",
                         geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)


# pulls black population uninsured percentages ----------------------------

#2019
b_unins_19 <- get_acs(geography = "county",
                             state = "VA",
                             county = county_fips,
                             year = 2019,
                             survey = "acs5",
                             variables = c("Black Population" = "S2701_C04_017"),
                             summary_var = "S2701_C01_017",
                             geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#2018
b_unins_18 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2018,
                      survey = "acs5",
                      variables = c("Black Population" = "S2701_C04_017"),
                      summary_var = "S2701_C01_017",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#2017
b_unins_17 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2017,
                      survey = "acs5",
                      variables = c("Black Population" = "S2701_C04_017"),
                      summary_var = "S2701_C01_017",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#2016
b_unins_16 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2016,
                      survey = "acs5",
                      variables = c("Black Population" = "S2701_C04_018"),
                      summary_var = "S2701_C01_018",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#2015
b_unins_15 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2015,
                      survey = "acs5",
                      variables = c("Black Population" = "S2701_C04_018"),
                      summary_var = "S2701_C01_018",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#2014
b_unins_14 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2014,
                      survey = "acs5",
                      variables = c("Black Population" = "S2701_C02_010"),
                      summary_var = "S2701_C01_010",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#2013
b_unins_13 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2013,
                      survey = "acs5",
                      variables = c("Black Population" = "S2701_C02_010"),
                      summary_var = "S2701_C01_010",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#2012
b_unins_12 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2012,
                      survey = "acs5",
                      variables = c("Black Population" = "S2701_C02_010"),
                      summary_var = "S2701_C01_010",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#2011
b_unins_11 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2011,
                      survey = "acs1",
                      variables = c("Black Population" = "S2701_C02_009"),
                      summary_var = "S2701_C01_009",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#2010
b_unins_10 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      year = 2010,
                      survey = "acs1",
                      variables = c("Black Population" =  "S2701_C02_009"),
                      summary_var = "S2701_C01_009",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)


# creates csv files for shiny ---------------------------------------------

#2019
unins_19 <- rbind(b_unins_19, hamp_unins_19)
write_csv(unins_19, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2019.csv")

#2018
unins_18 <- rbind(b_unins_18, hamp_unins_18)
write_csv(unins_18, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2018.csv")

#2017
unins_17 <- rbind(b_unins_17, hamp_unins_17)
write_csv(unins_17, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2017.csv")

#2016
unins_16 <- rbind(b_unins_16, hamp_unins_16)
write_csv(unins_16, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2016.csv")

#2015
unins_15 <- rbind(b_unins_15, hamp_unins_15)
write_csv(unins_15, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2015.csv")

#2014
unins_14 <- rbind(b_unins_14, hamp_unins_14)
write_csv(unins_14, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2014.csv")

#2013
unins_13 <- rbind(b_unins_13, hamp_unins_13)
write_csv(unins_13, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2013.csv")

#2012
unins_12 <- rbind(b_unins_12, hamp_unins_12)
write_csv(unins_12, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2012.csv")

#2011
unins_11 <- rbind(b_unins_11, hamp_unins_11)
write_csv(unins_11, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2011.csv")

#2010
unins_10 <- rbind(b_unins_10, hamp_unins_10)
write_csv(unins_10, file = "/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/shinyapp/data/TableS2701FiveYearEstimates/uninsured2010.csv")


# Plots Data -----------------------------------------------
#plots Hampton uninsured data for black population
hmp_black_bar <- hampton_black_cov %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = estimate)) + geom_col() +
  theme_minimal() +
  labs(title = "Hampton Roads: Black Uninsured Population",
       y = "Percent (%)",
       x = "Counties of Hampton Roads",
       caption = "Source: ACS 5 Year Estimate")

hmp_black_bar

#plots Hampton uninsured data for total population
hmp_tot_bar <- hampton_overall %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = estimate)) + geom_col() +
  theme_minimal() +
  labs(title = "Hampton Roads: Total Uninsured Population",
       y = "Percent (%)",
       x = "Counties of Hampton Roads",
       caption = "Source: ACS 5 Year Estimate")

hmp_tot_bar



#plots Virginia uninsured data for total population
va_tot_bar <- va_cov %>% 
  ggplot(aes(x = NAME, y = pct_tot_uninsured)) + geom_col() +
  theme_minimal() +
  labs(title = "Virginia: Total Uninsured Population",
       y = "Percent (%)",
       x = "Virginia",
       caption = "Source: ACS 5 Year Estimate")

va_tot_bar

# Plots Stacked Bar for Comparison---------------------------------------

stack_bar <- unins_19 %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(fill = variable, y = estimate, x = NAME)) +
  geom_bar(position = "stack", stat = "identity") +
    theme_minimal() +
    theme(legend.title = element_blank()) +
     labs(title = "",
       y = "Percent Uninsured (%)",
       x = "",
       caption = "Source: ACS 5 Year Estimate Table S2701") +
       theme(axis.text.x = element_text(angle = 40)) + 
    scale_fill_manual(values = c("#D55E00", "#0072B2"))

ggplotly(stack_bar)

