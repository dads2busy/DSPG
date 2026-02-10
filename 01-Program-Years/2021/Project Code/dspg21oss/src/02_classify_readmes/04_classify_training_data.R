

# create training sets 

#### load the data and functions #####################################################################################

rm(list=ls())
library("tidyverse")
library("dplyr")
library("readr")
library("RPostgreSQL")
#source("~/git/dspg21oss/scripts/detect_sw_sz.R")
#source("~/git/dspg21oss/scripts/detect_sw_co.R")
source("~/git/dspg21oss/scripts/detect_sw.R")

# repos/commits data 
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
repos_table <- dbGetQuery(conn, "SELECT *  FROM gh_2007_2020.repos_data;")
dbDisconnect(conn)

readme_subset_data <- repos_table  %>% 
  select(slug, description, readme, language, 
         topics, commits, forks, stars, watchers) %>%
  arrange(-stars) %>% 
  # clean the topics column 
  mutate(topics = str_replace_all(topics, "\\[", ""),
         topics = str_replace_all(topics, "\\]", ""),
         topics = str_replace_all(topics, "',", ""),
         topics = str_replace_all(topics, "'", "")) %>% 
  slice(1:10000) 

#### classify with descriptions #####################################################################################

classified_with_desciptions <- readme_subset_data %>% 
  # programming languages 
  detect_prog_gen_sw(slug, description) %>% 
  detect_prog_stat_sw(slug, description) %>% 
  detect_prog_web_sw(slug, description) %>% 
  # system software 
  detect_system_sw(slug, description) %>% 
  # ai/machine learning 
  detect_ai_sw(slug, description) %>%
  # data visualization 
  detect_viz_sw(slug, description) %>%
  # blockchain technologies
  detect_blockchain_sw(slug, description, sum_only = TRUE) %>%
  # business/office management
  detect_business_sw(slug, description, sum_only = TRUE) %>% 
  # database management
  detect_database_sw(slug, description, sum_only = TRUE) %>% 
  # parse the variables down to what we have finished 
  select(slug, description, readme, language, topics, commits, forks, stars, watchers, 
         #prog_python, prog_rlang, prog_java, prog_javascript, prog_php, prog_clang, prog_stat_all,
         sys_windows, sys_linux, sys_mac, sys_android, sys_virtual, 
         app_blockchain_all, app_business_all, app_database_all, topics_ai, topics_dataviz)

colSums(classified_with_desciptions %>% select(-slug, -description, -readme, -language, -topics))

data_viz <- output_data %>% filter(topics_dataviz == 1)

#### subset labeled data #####################################################################################

output_data <- classified_with_desciptions %>% 
  select(-readme) %>% 
  arrange(-commits) 

setwd("~/git/dspg21oss/data/dspg21oss/")
write_csv(output_data, "oss_software_tolabel_071621.csv")











