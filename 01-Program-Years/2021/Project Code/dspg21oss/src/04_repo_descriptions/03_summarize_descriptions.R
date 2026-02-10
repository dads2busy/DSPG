
rm(list=ls())
library("stringr")
library("tidyr")
library("dplyr")
library("readr")
library("RPostgreSQL")
source("~/git/dspg21oss/scripts/detect_sw.R")

# repos/commits data 
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
repos_table <- dbGetQuery(conn, "SELECT slug, description, primarylanguage as language, commits 
                          FROM gh_2007_2020.repos;")
dbDisconnect(conn)

descriptions_subset <- repos_table %>% 
  drop_na(description)

nrow(descriptions_subset) / nrow(repos_table)

# this takes about 22 hours to run (so we ran a slurm)
# you can pull in the results from rds in the 04_visualizaiton.R file
str_c("Started at: ", Sys.time())
#classified_with_desciptions <- repos_data %>% 
desciptions_classified <- descriptions_subset %>% 
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
  detect_database_sw(slug, description, sum_only = TRUE) 
str_c("Finished at: ", Sys.time())

setwd("/project/class/bii_sdad_dspg/uva_2021/dspg21oss/")
saveRDS(desciptions_classified, "descriptions_classified.rds")


