rm(list=ls())
# Load Libraries
library("dplyr")
library("readr")

# getting topics classified in dictionary
topics_df <- read_csv("~/github_topics_classified_070721.csv")
topics <- topics_df %>%
  filter(main_type=="Database")

topics <- topics$term
topics <- paste(topics, collapse="|")
# run this to print terms
str_replace_all(topics,"'", "")

# Read in readme data
path_for_data = "/project/class/bii_sdad_dspg/uva_2021/dspg21oss/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_071221.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)

# load function 
source("~/git/dspg21oss/scripts/detect_sw_co.R")

# using function to classify  
chk_sys <- readme_raw_data %>%
  top_n(1000, slug) %>% 
  detect_system_sw(slug, readme_text)
# check utility
chk_utility <- readme_raw_data %>%
  top_n(1000, slug) %>% 
  detect_utility_sw(slug, readme_text)
# check application
chk_app <- readme_raw_data %>%
  top_n(1000, slug) %>% 
  detect_application_sw(slug, readme_text)
# check database
chk_db <- readme_raw_data %>%
  top_n(1000, slug) %>% 
  detect_database_sw(slug, readme_text)
# check ai
chk_ai <- readme_raw_data %>%
  top_n(1000, slug) %>% 
  detect_ai_sw(slug, readme_text)
# check viz
chk_viz <- readme_raw_data %>%
  top_n(1000, slug) %>% 
  detect_viz_sw(slug, readme_text)

# 425 have at least 1, 299 over 5
sys_true <- chk_sys %>% 
  filter(system_all > 5)

# 84 have at least 1, 8 have over 5
util_true <- chk_utility %>% 
  filter(utility_all > 5)

# 487 have at least 1 ,188 over 5
app_true <- chk_app %>% 
  filter(app_all > 0)

# 30 have over 0, 12 have over 5
ai_true <- chk_ai %>% 
  filter(ai > 5)

# 27 have over 0, 1 has over 5
viz_true <- chk_viz %>% 
  filter(viz > 5)

# only one column at a time
# if you only want to develop certain categories 
system_terms <- get_dictionary_terms(summary_type = "System")
sys_os <- get_dictionary_terms(main_type = "Operating Systems")
windows_terms <- get_dictionary_terms(sub_type = "Windows")

chk <- readme_raw_data %>% 
  top_n(25, slug) %>% 
  as_tidytable() %>% 
  tidytable::mutate.(readme_text = tolower(readme_text)) %>% 
  detect_types(slug, readme_text, windows_terms) 
