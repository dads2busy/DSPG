# Loading ####
library(collapsibleTree)
library(tidyverse)
library(BBmisc)

# import data
setwd("~/git/dspg21oss/docs/")
df <- read_csv("oss_software_types - dictionary.csv")

# make tree ####

# filter out no counts and add in "Other" to correct counts

df_no_na <- df %>% 
  filter(!is.na(sourceforge_count)) %>% 
  filter(!is.na(fleming_primary)) %>%
  filter(!is.na(fleming_secondary)) %>%
  mutate(main_type = replace(main_type, main_type=="" & summary_type!="Programming" & summary_type!="Other/Nonlisted Topic", "Other")) %>% 
  mutate(sub_type = replace(sub_type, sub_type=="" & summary_type!="Programming" & summary_type!="Other/Nonlisted Topic", "Other"))


collapsibleTreeSummary(df,
                       hierarchy = c("fleming_primary", "fleming_secondary"),
                       width=800, height = 1000, 
                       root = "Software Types", 
                       fontSize = 20,
                       zoomable = FALSE, 
                       fillFun = colorspace::heat_hcl)


collapsibleTreeSummary(df_no_na,
                hierarchy = c("summary_type", "main_type", "sub_type"),
                width=800, height = 1000,  
                root = "Software Types", 
                fontSize = 20,
                zoomable = FALSE,
                attribute = "sourceforge_count",
                fillFun = colorspace::heat_hcl)
