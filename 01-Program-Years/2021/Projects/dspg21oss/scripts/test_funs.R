
##### stephanie
# put .R file in /scripts
# change .csv name 

# load packages and data 
rm(list=ls())
library("dplyr")
library("readr")
path_for_data = "/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_061521.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)

# load functions  
source("~/git/dspg21oss/scripts/detect_sw.R")

blockchain_projects <- readme_raw_data %>%
  detect_blockchain_sw(slug, readme_text) %>% 
  filter(app_blockchain_all > 0)
  
data(stop_words)

chk <- blockchain_projects %>% 
  #top_n(25, app_blockchain_all) %>%  
  unnest_tokens(word, readme_text) %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(-n)










# load functions  
source("~/git/dspg21oss/scripts/detect_sw_sz.R")

# using function to classify 
chk <- readme_raw_data %>%
  top_n(25, slug) %>% 
  detect_prog_stat_sw(slug, readme_text) 

# other functions 
chk <- readme_raw_data %>%
  top_n(25, slug) %>%  
  detect_prog_web_sw(slug, readme_text) %>% 
  detect_prog_gen_sw(slug, readme_text) 

# if you only want to develop certain categories 
#system_terms <- get_dictionary_terms(summary_type = "System")
#sys_os <- get_dictionary_terms(main_type = "Operating Systems")
windows_terms <- get_dictionary_terms(sub_type = "Windows")
chk <- readme_raw_data %>% 
  top_n(25, slug) %>% 
  as_tidytable() %>% 
  tidytable::mutate.(readme_text = tolower(readme_text)) %>% 
  detect_types(slug, readme_text, windows_terms) 


##### cierra 
# put .R file in /scripts
# change .csv name 

# load data 
rm(list=ls())
library("dplyr")
library("readr")
path_for_data = "/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_061521.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)

# load function 
source("~/git/dspg21oss/scripts/detect_sw_co.R")

# using function to classify  
chk <- readme_raw_data %>%
  top_n(25, slug) %>% 
  detect_system_sw(slug, readme_text)

# other functions 
chk <- readme_raw_data %>%
  top_n(25, slug) %>% 
  detect_utility_sw(slug, readme_text)

# if you only want to develop certain categories 
system_terms <- get_dictionary_terms(summary_type = "System")
sys_os <- get_dictionary_terms(main_type = "Operating Systems")
windows_terms <- get_dictionary_terms(sub_type = "Windows")
chk <- readme_raw_data %>% 
  top_n(25, slug) %>% 
  as_tidytable() %>% 
  tidytable::mutate.(readme_text = tolower(readme_text)) %>% 
  detect_types(slug, readme_text, windows_terms) 









# test get_dictionary_terms() 
source("~/git/dspg21oss/scripts/detect_sw.R")
chk_dict <- get_dictionary_terms(summary_type = "Application")
chk_dict <- get_dictionary_terms(main_type = "Blockchain")
chk_dict <- get_dictionary_terms(main_type = "Blockchain", sub_type = FALSE)
chk_dict <- get_dictionary_terms(sub_type = "Crytocurrency")
chk_dict <- get_dictionary_terms(summary_type = "Utility", main_type = FALSE, sub_type = FALSE)


# test detect_types() 
setwd("~/git/dspg21oss/docs/")
software_types <- readr::read_csv("oss_software_types - dictionary.csv") 
util_general <- software_types %>% filter(summary_type == "Utility" & is.na(main_type))
util_general <- as.data.frame(util_general$terms)
chk <- readme_raw_data %>% 
  detect_types(slug, readme_text, util_general)


# test detect_utility_sw() 
chk <- readme_raw_data %>% 
  detect_prog_stat_sw(slug, readme_text) %>% 
  detect_prog_web_sw(slug, readme_text)



# still in development 
tmp_df <- readme_raw_data %>% 
  as.data.frame() %>% 
  tidytext::unnest_tokens(word, readme_text) %>%
  as_tidytable() %>% 
  tidytable::count.(slug, name = "n_words")

chk <- chk %>% 
  tidytable::left_join.(tmp_df) %>% 
  tidytable::mutate.(sd_words = scale(n_words, center = TRUE, scale = TRUE)) %>% 
  tidytable::mutate.(utility_prob = as.numeric(utility_all / n_words)) 

chk_chk <- readme_raw_data %>% 
  tidytable::distinct.(readme_text) %>% 
  tidytable::mutate.(unique_text = 1) 

chk_chk_chk <- readme_raw_data %>% 
  tidytable::left_join.(chk_chk)






setwd("~/git/dspg21oss/docs/")
software_types <- readr::read_csv("oss_software_types - dictionary.csv", col_types = cols())
software_types %>% 
  unnest(terms = strsplit(terms, "\\|")) 










