# Plotting reported property values as overlaying histograms
# Fairfax county vs. CoreLogic

# load packages
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(data.table)
library(maditr)
library(scales)

# first load corelogic stuff for fairfax as ffx_2018
# now load data from fairfax
fairfax_housing_2018_geo <- read_csv("~/git/dspg20broadbandERS/data/fairfax-data/fairfax_housing_2018_geo.csv")

# combine data from fairfax and corelogic to make one data table
ffx_property_values <- data.table(total_value = na.omit(fairfax_housing_2018_geo$VALUE_TOTAL), data_source = "fairfax")
ffx_property_values <- add_row(ffx_property_values, total_value = as.numeric(na.omit(ffx_2018$assessed_total_value)), data_source = "corelogic")

# plot histograms
options(scipen = 10)
ggplot(ffx_property_values, aes(x = total_value)) +
  geom_histogram(data = subset(ffx_property_values, data_source == "fairfax"), aes(fill = "Fairfax"), alpha = 0.6) +
  geom_histogram(data = subset(ffx_property_values, data_source == "corelogic"), aes(fill = "CoreLogic"), alpha = 0.5) +
  theme_classic() +
  scale_fill_manual(values = c("CoreLogic" = "blue", "Fairfax" = "red")) +
  scale_x_log10(label = comma) +
  scale_y_log10(label = comma) +
  labs(x = "Property Value", y = "Frequency", title = "Property Values in Fairfax County", fill = "Dataset")
