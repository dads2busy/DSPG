library(data.table)
library(maditr)

# load packages and data 
rm(list=ls())
library("dplyr")
library("readr")
library(stringr)
path_for_data = "/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/"
setwd(path_for_data)

classified<-read_csv("dspg21oss/github_topics_classified_070721.csv") 
classified_programming_python<-classified %>% filter(summary_type=="Programming" & sub_type=="Python")
classified_programming_python <- classified_programming_python$term
classified_programming_python <- paste(classified_programming_python, collapse="|")
# run this to print terms
str_replace_all(classified_programming_python,"'", "")


readme_raw_data <- read_csv("oss_readme_data_061521.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)
# load functions  
source("~/git/dspg21oss/scripts/detect_sw_sz.R")
# using function to classify 
chk <- readme_raw_data %>%
  top_n(50, slug) %>% 
  detect_prog_stat_sw(slug, readme_text) 
# other functions 
chk <- readme_raw_data %>%
  top_n(25, slug) %>%  
  detect_prog_web_sw(slug, readme_text) #%>% 
  #detect_prog_gen_sw(slug, readme_text) 
# if you only want to develop certain categories 
#system_terms <- get_dictionary_terms(summary_type = "System")
#sys_os <- get_dictionary_terms(main_type = "Operating Systems")

#one column at a time (use above for more columns at a time)
windows_terms <- get_dictionary_terms(sub_type = "Windows")
chk <- readme_raw_data %>% 
  top_n(25, slug) %>% 
  as_tidytable() %>% 
  tidytable::mutate.(readme_text = tolower(readme_text)) %>% 
  detect_types(slug, readme_text, windows_terms) 
