# Plotting reported property values as overlaying histograms
# New Kent county vs. CoreLogic

# load packages
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(data.table)
library(maditr)
library(scales)

# first load corelogic stuff for new kent as nk_2018
# now load data from new kent
new_kent <- read_excel("~/NK_ASSESSMENT_REPORT_2020.xlsx")

# combine data from new kent and corelogic to make one data table
nk_property_values <- data.table(total_value = na.omit(new_kent$Total), data_source = "new_kent")
nk_property_values <- add_row(nk_property_values, total_value = as.numeric(na.omit(nk_2018$assessed_total_value)), data_source = "corelogic")

# plot histograms
options(scipen = 10)
ggplot(nk_property_values, aes(x = total_value)) +
  geom_histogram(data = subset(nk_property_values, data_source == "new_kent"), aes(fill = "New Kent"), alpha = 0.6) +
  geom_histogram(data = subset(nk_property_values, data_source == "corelogic"), aes(fill = "CoreLogic"), alpha = 0.5) +
  theme_classic() +
  scale_fill_manual(values = c("CoreLogic" = "blue", "New Kent" = "red")) +
  scale_x_log10(label = comma) +
  scale_y_log10(label = comma) +
  labs(x = "Property Value", y = "Frequency", title = "Property Values in New Kent County", fill = "Dataset")
