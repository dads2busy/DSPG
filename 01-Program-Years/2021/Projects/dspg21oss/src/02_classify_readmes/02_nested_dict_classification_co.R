# load packages and ingest your readme data  #############################################################################

library(tidyverse)
library(data.table)
library(maditr)
library(RPostgreSQL)

try <- read_csv("/project/class/bii_sdad_dspg/ncses_oss_2021/requests_scrape/oss_readme_aggregated/oss_readme_data_062121.csv")

# ingest data locally from csv 
path_for_data = "~/git/dspg21oss/data/dspg21oss"
setwd(path_for_data)
readme_raw_data <- read_csv("readme_test_data.csv") %>% 
  filter(status == "Done") %>% 
  distinct(slug, readme_text, batch, as_of, status)

# # or grab data from postgresql 
# conn <- dbConnect(drv = PostgreSQL(), dbname = "sdad", 
#                   host = "10.250.124.195", port = 5432, 
#                   user = Sys.getenv("db_userid"), 
#                   password = Sys.getenv("db_pwd"))
# readme_raw_data <- dbGetQuery(conn, "select * from gh_2007_2020.readme_data;")
# dbDisconnect(conn)


# ingested dictionary, filter to categories, convert to strings  #########################################################

# ingest nested dictionary
#software_types <- read_csv("~/oss_software_types - dictionary.csv") 
software_types <- read_csv("~/git/dspg21oss/docs/oss_software_types - dictionary - co.csv")

# based on summary type 
utility <- software_types %>% filter(summary_type == "Utility")
utility <- paste(c("\\b(?i)(zcx", utility$terms, "zcx)\\b"), collapse = "|")
application <- software_types %>% filter(summary_type == "Application")
application <- paste(c("\\b(?i)(zcx", application$terms, "zcx)\\b"), collapse = "|")
programming <- software_types %>% filter(summary_type == "Programming")
programming <- paste(c("\\b(?i)(zcx", programming$terms, "zcx)\\b"), collapse = "|")
system <- software_types %>% filter(summary_type == "System")
system <- paste(c("\\b(?i)(zcx", system$terms, "zcx)\\b"), collapse = "|")
topics <- software_types %>% filter(summary_type == "Topics")
topics <- paste(c("\\b(?i)(zcx", topics$terms, "zcx)\\b"), collapse = "|")

# based on main_type 
# utility
security <- software_types %>% filter(main_type == "Security")
security<- paste(c("\\b(?i)(zcx", security$terms, "zcx)\\b"), collapse = "|")
# application
blockchain <- software_types %>% filter(main_type == "Blockchain")
blockchain <- paste(c("\\b(?i)(zcx", blockchain$terms, "zcx)\\b"), collapse = "|")
communications <- software_types %>% filter(main_type == "Communications")
communications <- paste(c("\\b(?i)(zcx", communications$terms, "zcx)\\b"), collapse = "|")
database <- software_types %>% filter(main_type == "Database")
database <- paste(c("\\b(?i)(zcx", database$terms, "zcx)\\b"), collapse = "|")
desktop_environments <- software_types %>% filter(main_type == "Desktop Environments")
desktop_environments <- paste(c("\\b(?i)(zcx", desktop_environments$terms, "zcx)\\b"), collapse = "|")
education <- software_types %>% filter(main_type == "Education")
education <- paste(c("\\b(?i)(zcx", education$terms, "zcx)\\b"), collapse = "|")
formats_protocols <- software_types %>% filter(main_type == "Formats/Protocols")
formats_protocols <- paste(c("\\b(?i)(zcx", formats_protocols$terms, "zcx)\\b"), collapse = "|")
games_entertainment <- software_types %>% filter(main_type == "Games/Entertainment")
games_entertainment <- paste(c("\\b(?i)(zcx", games_entertainment$terms, "zcx)\\b"), collapse = "|")
internet <- software_types %>% filter(main_type == "Internet")
internet <- paste(c("\\b(?i)(zcx", internet$terms, "zcx)\\b"), collapse = "|")
mobile <- software_types %>% filter(main_type == "Mobile")
mobile <- paste(c("\\b(?i)(zcx", mobile$terms, "zcx)\\b"), collapse = "|")
multimedia <- software_types %>% filter(main_type == "Multimedia")
multimedia <- paste(c("\\b(?i)(zcx", multimedia$terms, "zcx)\\b"), collapse = "|")
office_business <- software_types %>% filter(main_type == "Office/Business")
office_business <- paste(c("\\b(?i)(zcx", office_business$terms, "zcx)\\b"), collapse = "|")
printing <- software_types %>% filter(main_type == "Printing")
printing <- paste(c("\\b(?i)(zcx", printing$terms, "zcx)\\b"), collapse = "|")
text_editors <- software_types %>% filter(main_type == "Text Editors")
text_editors <- paste(c("\\b(?i)(zcx", text_editors$terms, "zcx)\\b"), collapse = "|")
# system
operating_systems <- software_types %>% filter(main_type == "Operating Systems")
operating_systems <- paste(c("\\b(?i)(zcx", operating_systems$terms, "zcx)\\b"), collapse = "|")
terminals <- software_types %>% filter(main_type == "Terminals")
terminals <- paste(c("\\b(?i)(zcx", terminals$terms, "zcx)\\b"), collapse = "|")
# topics
religion_phil <- software_types %>% filter(main_type == "Religion/Philosophy")
religion_phil <- paste(c("\\b(?i)(zcx", religion_phil$terms, "zcx)\\b"), collapse = "|")
scientific_eng <- software_types %>% filter(main_type == "Scientific/Engineering")
scientific_eng <- paste(c("\\b(?i)(zcx", scientific_eng$terms, "zcx)\\b"), collapse = "|")
social_sciences <- software_types %>% filter(main_type == "Social Sciences")
social_sciences <- paste(c("\\b(?i)(zcx", social_sciences$terms, "zcx)\\b"), collapse = "|")
software_development <- software_types %>% filter(main_type == "Software Development")
software_development <- paste(c("\\b(?i)(zcx", software_development$terms, "zcx)\\b"), collapse = "|")



# development space (so you don't have to do refresh the csv with each update) ###########################################

# it gets annoying to go back to that csv each time so you can just add words to the string and then rerun your function 
cryptocurrency <- software_types %>% filter(sub_type == "Crytocurrency")
cryptocurrency$terms <- paste(cryptocurrency$terms, 
                              "ethereum|ripple|cardano|litecoin|monero|tether|vechain|zcash|omisego|binance coin|bytecoin",
                              sep = "|")
streaming <- software_types %>% filter(sub_type == "Streaming")
streaming$terms <- paste(streaming$terms, 
                         "wirecast|vmix|vidblasterx|obs studio|streamlabs obs|ffmpeg|xsplit broadcaster|splitcam|restream studio",
                         sep = "|")
file_sharing <- software_types %>% filter(sub_type == "File Sharing")
file_sharing$terms <- paste(file_sharing$terms, 
                            "pcloud|wetransfer|imgur|onehub|mediafire",
                            sep = "|")
chat <- software_types %>% filter(sub_type == "Chat")
chat$terms <- paste(chat$terms, 
                            "chatsdk|chat21|firechat|messagekit|rocket chat|zulipchat|meteor",
                            sep = "|")
email <- software_types %>% filter(sub_type == "Email")
email$terms <- paste(email$terms, 
                     "gnu mailman|listmessenger|mailchimp|mautic|openemm|phplist|sendblaster",
                    sep = "|")


database_servers <- paste(c("\\b(?i)(zcx", database_servers$term, 
                            "database mangement|databases|postgresql|postgres",
                            "zcx)\\b"), collapse = "|")

# from source forge

windows_terms <- c("windows","antimicro", "scptoolkit", "scrollout f1", "7-zip", "scrcpy","rufus",
                   "turbovnc","etcher", "desmume","webmin","libusb","gparted","win32 disk imager",
                   "windows directory statistics", "net-snmp","ds4windows","asuswrt-merlin",
                   "gpt fdisk","process hacker", "visualboyadvance","darik's book and nuke","opentrack",
                   "ventoy", "vjoy", "s.m.a.r.t. monitoring tools","nullsoft scriptable","ophcrack",
                   "v2ray","double commander","catacombae","reactos","ext2","boot-repair-disk",
                   "blissos-dev","waircut","nikkho","refind","magisk","ext2read","angry ip scanner",
                   "iperf","grub2win","gns3","nestopia","algo vpn")

linux_terms <- c("linux","antimicro","scrollout f1","ubuntuzilla","scrcpy","rufus","turbovnc","autoap","etcher",
                 "desmume","webmin","libusb","gparted","intel ethernet","zorin","xz utils","openmediavault",
                 "mx-linux","net-snmp","cracklib","asuswrt-merlin","gpt fdisk","visualboyadvance",
                 "darik's book and nuke","opentrack","ventoy","s.m.a.r.t. monitoring tools","ext2",
                 "hdparm","clonezilla","squashfs","ophcrack","v2ray","double commander","catacombae",
                 "reactos","p7zip","boot-repair-disk","blissos-dev","refind","procps-ng")

mac_terms <- c("mac","scrollout f1","scrcpy","turbovnc","info-zip project","webmin","desmume","libusb","gparted",
               "etcher","asuswrt-merlin","gpt fdisk","darik's book and nuke","opentrack","visualboyadvance",
               "s.m.a.r.t. monitoring tools","reactos","double commander","catacombae","v2ray","blissos-dev",
               "boot-repair-disk","angry ip scanner","refind","magisk","iperf","gns3","zabbix","macos","algo vpn",
               "grandperspective","retroarch","docfetcher","iometer","veracrypt","boch x86 pc emulator","ghostscript fonts",
               "drawio-desktop","gsmartcontrol","powershell","lxpup","gutenprint","voodoohda","macintosh os")

# write terms to csv
  

# and then if you want to output your updated string and paste it back to the csv you can do it with this 
vector_to_unnest <- "database mangement|databases"
vector_to_unnest <- data.frame(unlist(strsplit(vector_to_unnest, "\\|")))
colnames(vector_to_unnest) <- "terms"
write_csv(vector_to_unnest, str_c(path_for_data, "terms_to_update.csv"))

# based on sub_type ####
# cryptocurrency <- software_types %>% filter(sub_type == "Crytocurrency")
cryptocurrency <- paste(c("\\b(?i)(zcx", cryptocurrency$terms, "zcx)\\b"), collapse = "|")
database_servers <- software_types %>% filter(sub_type == "Database Engines/Servers")
database_servers <- paste(c("\\b(?i)(zcx", database_servers$terms, "zcx)\\b"), collapse = "|")
streaming <- paste(c("\\b(?i)(zcx", streaming$terms, "zcx)\\b"), collapse = "|")
file_sharing <- paste(c("\\b(?i)(zcx", file_sharing$terms, "zcx)\\b"), collapse = "|")
chat <- paste(c("\\b(?i)(zcx", chat$terms, "zcx)\\b"), collapse = "|")
email <- paste(c("\\b(?i)(zcx", email$terms, "zcx)\\b"), collapse = "|")

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
  # OR classify the categories as binary variables in a matrix 
  dt_mutate(security = ifelse(test = str_detect(string = readme_text, pattern = security), 1, 0)) %>%
  dt_mutate(blockchain = ifelse(test = str_detect(string = readme_text, pattern = blockchain), 1, 0)) %>%
  dt_mutate(communications = ifelse(test = str_detect(string = readme_text, pattern = communications), 1, 0)) %>% 
  dt_mutate(database = ifelse(test = str_detect(string = readme_text, pattern = database), 1, 0)) %>%
  dt_mutate(desktop_environments = ifelse(test = str_detect(string = readme_text, pattern = desktop_environments), 1, 0)) %>%
  dt_mutate(education = ifelse(test = str_detect(string = readme_text, pattern = education), 1, 0)) %>%
  dt_mutate(formats_protocols = ifelse(test = str_detect(string = readme_text, pattern = formats_protocols), 1, 0)) %>%
  dt_mutate(games_entertainment = ifelse(test = str_detect(string = readme_text, pattern = games_entertainment), 1, 0)) %>%
  dt_mutate(internet = ifelse(test = str_detect(string = readme_text, pattern = internet), 1, 0)) %>%
  dt_mutate(mobile = ifelse(test = str_detect(string = readme_text, pattern = mobile), 1, 0)) %>%
  dt_mutate(multimedia = ifelse(test = str_detect(string = readme_text, pattern = multimedia), 1, 0)) %>%
  dt_mutate(office_business = ifelse(test = str_detect(string = readme_text, pattern = office_business), 1, 0)) %>%
  dt_mutate(printing = ifelse(test = str_detect(string = readme_text, pattern = printing), 1, 0)) %>%
  dt_mutate(text_editors = ifelse(test = str_detect(string = readme_text, pattern = text_editors), 1, 0)) %>% 
  dt_mutate(operating_systems = ifelse(test = str_detect(string = readme_text, pattern = operating_systems), 1, 0)) %>%
  dt_mutate(terminals = ifelse(test = str_detect(string = readme_text, pattern = terminals), 1, 0)) %>%
  dt_mutate(religion_phil = ifelse(test = str_detect(string = readme_text, pattern = religion_phil), 1, 0)) %>%
  dt_mutate(scientific_eng = ifelse(test = str_detect(string = readme_text, pattern = scientific_eng), 1, 0)) %>%
  dt_mutate(social_sciences = ifelse(test = str_detect(string = readme_text, pattern = social_sciences), 1, 0)) %>%
  dt_mutate(software_development = ifelse(test = str_detect(string = readme_text, pattern = software_development), 1, 0))

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


# references 
# https://github.com/brandonleekramer/diversity/blob/master/src/02_text_trends/04_hypothesis4.R
# https://github.com/uva-bi-sdad/oss-2020/blob/master/src/07_ncses-indicators/02_intl_collaborations/02_github-users-to-ctry.Rmd 


