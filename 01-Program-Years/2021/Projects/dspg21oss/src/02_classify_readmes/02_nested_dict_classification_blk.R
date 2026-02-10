
# load packages and ingest your readme data  #############################################################################

library(tidyverse)
library(data.table)
library(maditr)
library(RPostgreSQL)

# ingest data locally from csv 
path_for_data = "/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/"
setwd(path_for_data)
readme_raw_data <- read_csv("oss_readme_data_062321.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)
           
# or grab data from postgresql 
#conn <- dbConnect(drv = PostgreSQL(), dbname = "sdad", 
#                  host = "10.250.124.195", port = 5432, 
#                  user = Sys.getenv("db_userid"), 
#                  password = Sys.getenv("db_pwd"))
#readme_raw_data <- dbGetQuery(conn, "select * from gh_2007_2020.readme_data;")
#dbDisconnect(conn)

 
# ingested dictionary, filter to categories, convert to strings  #########################################################

# ingest nested dictionary
setwd(path_for_data)
software_types <- read_csv("oss_software_types - dictionary.csv") 

# based on summary type 
utility <- software_types %>% filter(summary_type == "Utility")
utility <- paste(c("\\b(?i)(zcx", utility$term, "zcx)\\b"), collapse = "|")

# based on main_type 
blockchain <- software_types %>% filter(main_type == "Blockchain")
blockchain <- paste(c("\\b(?i)(zcx", blockchain$term, "zcx)\\b"), collapse = "|")

communications <- software_types %>% filter(main_type == "Communications")
communications <- paste(c("\\b(?i)(zcx", communications$term, "zcx)\\b"), collapse = "|")

# based on sub_type 
database_servers <- software_types %>% filter(sub_type == "Database Engines/Servers")
# database_servers <- paste(c("\\b(?i)(zcx", database_servers$term, "zcx)\\b"), collapse = "|")


# development space (so you don't have to do refresh the csv with each update) ###########################################

# it gets annoying to go back to that csv each time so you can just add words to the string and then rerun your function 
database_servers <- paste(c("\\b(?i)(zcx", database_servers$term, 
                            "database management|solarwinds database|dbvisualizer|manageengine|oracle rdbms|ibm db2|microsoft sql|sap sybase ase|teradata|adabas|mysql|filemaker|microsoft access|informix|sqlite|postgresql|amazonrds|mongodb|rdeis|couchdb|neo4j|couchbase|orientdb|phpmyadmin|sql developer|seqel pro|robomongo|hadoop hdfs|cloudera|mariadb|informix dynamic server|altibase",
                            "zcx)\\b"), collapse = "|")

# and then if you want to output your updated string and paste it back to the csv you can do it with this 
vector_to_unnest <- "database mangement|databases"
vector_to_unnest <- data.frame(unlist(strsplit(vector_to_unnest, "\\|")))
colnames(vector_to_unnest) <- "terms"
write_csv(vector_to_unnest, str_c(path_for_data, "terms_to_update.csv"))


# preprocessing ##########################################################################################################

readme_processed <- readme_raw_data %>% 
  mutate(readme_text = tolower(readme_text))
# NOTE: THIS STEP NEEDS WAY MORE WORK 

# classification #########################################################################################################

readme_classified <- readme_processed %>% 
  # first turn this into a data.table object for speed 
  as.data.table() %>% 
  # then you can EITHER classify all categories into one columns separated by commas 
  #dt_mutate(cats_as_text = ifelse(test = str_detect(string = readme_text, pattern = blockchain), 
  #                                yes = "blockchain", no = "")) %>%
  #dt_mutate(cats_as_text = ifelse(test = str_detect(string = readme_text, pattern = communications), 
  #                                paste("communications", cats_as_text, sep=", "), no = cats_as_text)) %>%
  # OR classify the categories as binary variables in a matrix 
  dt_mutate(blockchain = ifelse(test = str_detect(string = readme_text, pattern = blockchain), 1, 0)) %>%
  dt_mutate(communications = ifelse(test = str_detect(string = readme_text, pattern = communications), 1, 0))

# summary stats ##########################################################################################################

# check the totals across all columns 
readme_classified %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE))

# output data #############################################################################################################

# either locally using an rds file 
setwd(path_for_data)
write_rds(readme_classified, "readmes_classified.rds")

# or using the postgresql database 
conn <- dbConnect(drv = PostgreSQL(), dbname = "sdad", 
                  host = "10.250.124.195", port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
dbWriteTable(conn, c("gh_2007_2020", "readmes_classified"), 
             readme_classified, row.names = FALSE)
dbDisconnect(conn)

# links to help classify 
# https://www.softwaretestinghelp.com/database-management-software/ 


# references 
# https://github.com/brandonleekramer/diversity/blob/master/src/02_text_trends/04_hypothesis4.R
# https://github.com/uva-bi-sdad/oss-2020/blob/master/src/07_ncses-indicators/02_intl_collaborations/02_github-users-to-ctry.Rmd 