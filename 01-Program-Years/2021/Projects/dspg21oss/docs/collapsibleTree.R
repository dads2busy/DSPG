# Loading ####
library(collapsibleTree)
library(tidyverse)
library(BBmisc)

# import data
df <- read.csv("~/oss_software_types - dictionary.csv")
sw_types <- select(df,"summary_type", "main_type", "sub_type")

# make tree ####

# filter out no counts and add in "Other" to correct counts

df_no_na <- df %>% 
      filter(!is.na(sourceforge_count)) %>% 
      mutate(main_type = replace(main_type, main_type=="" & summary_type!="Programming" & summary_type!="Other/Nonlisted Topic", "Other")) %>% 
      mutate(sub_type = replace(sub_type, sub_type=="" & summary_type!="Programming" & summary_type!="Other/Nonlisted Topic", "Other"))


collapsibleTreeSummary(df_no_na,
                hierarchy = c("summary_type", "main_type", "sub_type"),
                width=800, height = 850, 
                root = "Software Types", 
                fontSize = 10,
                attribute = "sourceforge_count",
                fillFun = colorspace::heat_hcl)
