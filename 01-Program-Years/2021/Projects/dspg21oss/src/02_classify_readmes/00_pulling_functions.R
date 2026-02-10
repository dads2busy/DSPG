

rm(list=ls())
library("dplyr")
library("readr")
source("~/git/dspg21oss/scripts/detect_sw.R")

path_for_data = "/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_061521.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)

chk <- readme_raw_data %>% 
  #detect_blockchain_sw(readme_text) %>% 
  detect_utility_sw(readme_text, sum_only = TRUE) %>% 
  detect_system_sw(readme_text, sum_only = TRUE)