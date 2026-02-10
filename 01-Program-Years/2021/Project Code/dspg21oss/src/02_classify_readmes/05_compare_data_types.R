
# this was created by brandon to compare the data 
# types but don't run this bc it will take days to run

# create training sets 

#### load the data and functions #####################################################################################

rm(list=ls())
library("tidyverse")
library("dplyr")
library("readr")
library("RPostgreSQL")
#source("~/git/dspg21oss/scripts/detect_sw_sz.R")
source("~/git/dspg21oss/scripts/detect_sw_co.R")
source("~/git/dspg21oss/scripts/detect_sw.R")

# readme data 
path_for_data = "/project/class/bii_sdad_dspg/uva_2021/dspg21oss/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_071221.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)

# repos/commits data 
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
repos_table <- dbGetQuery(conn, "SELECT slug, description, primarylanguage as language, commits 
                          FROM gh_2007_2020.repos WHERE commits > 349;")
dbDisconnect(conn)

# repo_stats data 
setwd(path_for_data)
repo_stats <- read_csv("repo_stats_0714.csv")

readme_subset_data <- repos_table  %>% 
  left_join(repo_stats, by = "slug") %>% 
  left_join(readme_raw_data, by = "slug") %>% 
  select(slug, description, readme_text, language, 
         topics, commits, forks, stars, watchers) %>%
  # clean the topics column 
  mutate(topics = str_replace_all(topics, "\\[", ""),
         topics = str_replace_all(topics, "\\]", ""),
         topics = str_replace_all(topics, "',", ""),
         topics = str_replace_all(topics, "'", "")) %>% 
  drop_na(description) %>% 
  slice(1:10000) 

#### classify with descriptions #####################################################################################

classified_with_desciptions <- readme_subset_data %>% 
  # programming languages 
  detect_prog_stat_sw(slug, description) %>% 
  detect_prog_web_sw(slug, description) %>% 
  # system software 
  detect_system_sw(slug, description) %>% 
  # blockchain technologies
  detect_blockchain_sw(slug, description, sum_only = TRUE) %>%
  # business/office management
  detect_business_sw(slug, description, sum_only = TRUE) %>% 
  # database management
  detect_database_sw(slug, description, sum_only = TRUE) %>% 
  # parse the variables down to what we have finished 
  select(slug, prog_python, prog_scala, prog_rlang, prog_julia, prog_stat_all,
         prog_javascript, prog_java, prog_typescript, prog_kotlin, prog_lua, prog_web_all,
         sys_windows, sys_linux, sys_mac, sys_android, sys_virtual, starts_with("app_")) %>% 
  rename_all(paste0, "_desc") %>% 
  rename(slug = slug_desc)

#### classify with coding langs #####################################################################################

classified_with_primarylang <- readme_subset_data %>% 
  # programming languages 
  detect_prog_stat_sw(slug, language) %>% 
  detect_prog_web_sw(slug, language) %>% 
  # system software 
  detect_system_sw(slug, language) %>% 
  # database management
  detect_database_sw(slug, language, sum_only = TRUE) %>% 
  # blockchain tecnologies
  detect_blockchain_sw(slug, language, sum_only = TRUE) %>%
  # business/office management
  detect_business_sw(slug, language, sum_only = TRUE) %>%
  # parse the variables down to what we have finished 
  select(slug, prog_python, prog_scala, prog_rlang, prog_julia, prog_stat_all,
         prog_javascript, prog_java, prog_typescript, prog_kotlin, prog_lua, prog_web_all,
         sys_windows, sys_linux, sys_mac, sys_android, sys_virtual, starts_with("app_")) %>% 
  rename_all(paste0, "_lang") %>% 
  rename(slug = slug_lang)

#### classify with readmes #####################################################################################

classified_with_readmes <- readme_subset_data %>% 
  # programming languages 
  detect_prog_stat_sw(slug, readme_text) %>% 
  detect_prog_web_sw(slug, readme_text) %>% 
  # system software 
  detect_system_sw(slug, readme_text) %>% 
  # database management
  detect_database_sw(slug, readme_text, sum_only = TRUE) %>% 
  # blockchain tecnologies
  detect_blockchain_sw(slug, readme_text, sum_only = TRUE) %>%
  # business/office management
  detect_business_sw(slug, readme_text, sum_only = TRUE) %>%
  # parse the variables down to what we have finished 
  select(slug, prog_python, prog_scala, prog_rlang, prog_julia, prog_stat_all,
         prog_javascript, prog_java, prog_typescript, prog_kotlin, prog_lua, prog_web_all,
         sys_windows, sys_linux, sys_mac, sys_android, sys_virtual, starts_with("app_")) %>% 
  rename_all(paste0, "_read") %>% 
  rename(slug = slug_read)

#### classify with topics #####################################################################################

classified_with_topics <- readme_subset_data %>% 
  # programming languages 
  detect_prog_stat_sw(slug, topics) %>% 
  detect_prog_web_sw(slug, topics) %>% 
  # system software 
  detect_system_sw(slug, topics) %>% 
  # database management
  detect_database_sw(slug, topics, sum_only = TRUE) %>% 
  # blockchain tecnologies
  detect_blockchain_sw(slug, topics, sum_only = TRUE) %>%
  # business/office management
  detect_business_sw(slug, topics, sum_only = TRUE) %>%
  # parse the variables down to what we have finished 
  select(slug, prog_python, prog_scala, prog_rlang, prog_julia, prog_stat_all,
         prog_javascript, prog_java, prog_typescript, prog_kotlin, prog_lua, prog_web_all,
         sys_windows, sys_linux, sys_mac, sys_android, sys_virtual, starts_with("app_")) %>% 
  rename_all(paste0, "_tops") %>% 
  rename(slug = slug_tops)

#### join dataframes #####################################################################################

all_classified_data <- readme_subset_data %>% 
  left_join(classified_with_desciptions, by = "slug") %>%
  left_join(classified_with_primarylang, by = "slug") %>% 
  left_join(classified_with_readmes, by = "slug") %>% 
  left_join(classified_with_topics, by = "slug") 

all_classified_data %>% 
  mutate(python = prog_python_desc + prog_python_lang + prog_python_read + prog_python_tops,
         javascript = prog_python_desc + prog_python_lang + prog_python_read + prog_python_tops,
         javascript = prog_python_desc + prog_python_lang + prog_python_read + prog_python_tops,
         javascript = prog_python_desc + prog_python_lang + prog_python_read + prog_python_tops,
         javascript = prog_python_desc + prog_python_lang + prog_python_read + prog_python_tops,
         javascript = prog_python_desc + prog_python_lang + prog_python_read + prog_python_tops)

#### stat prog langs #####################################################################################

chk <- all_classified_data %>% 
  #select(slug, description, readme_text, starts_with("prog_python")) %>% 
  filter(prog_python_desc == 1 | prog_python_lang == 1 | prog_python_read == 1 | prog_python_tops == 1 ) %>%
  mutate(python = prog_python_desc + prog_python_lang + prog_python_read + prog_python_tops,
         javascript = prog_javascript_desc + prog_javascript_lang + prog_javascript_read + prog_javascript_tops,
         blockchain = app_blockchain_all_desc + app_blockchain_all_lang + app_blockchain_all_read + app_blockchain_all_tops,
         android = sys_android_desc + sys_android_lang + sys_android_read + sys_android_tops,
         business = app_business_all_desc + app_business_all_lang + app_business_all_read + app_business_all_tops)
  
  
colSums(python_data %>% select(-slug, -description, -readme_text))

javascript_data <- all_classified_data %>% 
  #select(slug, description, readme_text, starts_with("prog_javascript")) %>% 
  filter(prog_javascript_desc == 1 | prog_javascript_lang == 1 | 
           prog_javascript_read == 1 | prog_javascript_tops == 1)
colSums(javascript_data %>% select(-slug, -description, -readme_text))

blockchain_data <- all_classified_data %>% 
  #select(slug, description, readme_text, starts_with("app_blockchain_all_")) %>% 
  filter(app_blockchain_all_desc == 1 | app_blockchain_all_lang == 1 | 
           app_blockchain_all_read == 1 | app_blockchain_all_tops == 1)
colSums(blockchain_data %>% select(-slug, -description, -readme_text))

android_data <- all_classified_data %>% 
  #select(slug, description, readme_text, starts_with("sys_android_")) %>% 
  filter(sys_android_desc == 1 | sys_android_lang == 1 | 
           sys_android_read == 1 | sys_android_tops == 1 )
colSums(android_data %>% select(-slug, -description, -readme_text))

business_data <- all_classified_data %>% 
  #select(slug, description, readme_text, starts_with("sys_android_")) %>% 
  filter(app_business_all_desc == 1 | app_bus_dsktoppub_lang == 1 | 
           app_bus_dsktoppub_read == 1 | app_bus_dsktoppub_tops == 1)
colSums(android_data %>% select(-slug, -description, -readme_text))

temp_data <- bind_rows(python_data, javascript_data, blockchain_data, android_data, business_data) %>% 
  distinct(slug, description, readme_text, language, commits, forks, stars, watchers, 
           
           )

setwd(path_for_data)
write_csv(temp_data, "oss_software_tolabel_071321.csv")


## not generating anything yet 

r_data <- all_classified_data %>% 
  select(slug, description, readme_text, starts_with("prog_rlang")) %>% 
  filter(prog_rlang_desc == 1 | prog_rlang_lang == 1 | prog_rlang_read == 1 )

colSums(r_data %>% select(-slug, -description, -readme_text))

scala_data <- all_classified_data %>% 
  select(slug, description, readme_text, starts_with("prog_scala")) %>% 
  filter(prog_scala_desc == 1 | prog_scala_lang == 1 | prog_scala_read == 1 )

colSums(scala_data %>% select(-slug, -description, -readme_text))

julia_data <- all_classified_data %>% 
  select(slug, description, readme_text, starts_with("prog_julia")) %>% 
  filter(prog_julia_desc == 1 | prog_julia_lang == 1 | prog_julia_read == 1 )

colSums(julia_data %>% select(-slug, -description, -readme_text))

java_data <- all_classified_data %>% 
  select(slug, description, readme_text, starts_with("prog_java")) %>% 
  filter(prog_java_desc == 1 | prog_java_lang == 1 | prog_java_read == 1 )

colSums(java_data %>% select(-slug, -description, -readme_text))

kotlin_data <- all_classified_data %>% 
  select(slug, description, readme_text, starts_with("prog_kotlin")) %>% 
  filter(prog_kotlin_desc == 1 | prog_kotlin_lang == 1 | prog_kotlin_read == 1 )

colSums(kotlin_data %>% select(-slug, -description, -readme_text))


#### to do later #####################################################################################

# not ready until after wednesday (july 15th, 2021)
classified_with_topics %>%
  # database management  
  detect_database_sw(slug, readme_text) %>% 
  # ai/machine learning 
  detect_aiml_sw(slug, readme_text) %>% 
  # data visualization 
  detect_dataviz_sw(slug, readme_text)


#### subsetting the training sets #####################################################################################

python_examples <- classified_with_topics %>% 
  filter(prog_python == 1) %>% 
  distinct(readme_text)


















