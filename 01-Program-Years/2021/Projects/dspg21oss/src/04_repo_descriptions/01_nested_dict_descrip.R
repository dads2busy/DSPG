#### 0: packages
library(RPostgres)
library(data.table)
library(tidytable)
library(ggplot2)
library(stringr)
library(tidytext)
library(cld3)
library(cld2)
library(textcat)
#library(tidyverse)
library(maditr)
library(extrafont)
library(stargazer)


#### 1: database table(s)

conn <- dbConnect(drv = Postgres(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv(x="db_userid"), 
                  password = Sys.getenv(x="db_pwd"))

raw_data <- dbGetQuery(conn, "select description, primarylanguage from gh_2007_2020.repos;")

dbDisconnect(conn=conn)
raw_data<-as.data.table(raw_data)
raw_data_backup<-raw_data

#raw_data<-raw_data_backup

software_types <- fread("docs/oss_software_types - dictionary.csv") 

#### 2: coding language
code_lang_count<-as.data.table(table(raw_data$primarylanguage)) %>% arrange.(desc(N))
head(code_lang_count, n=12)


#TO-DO: MAKE VIZ

stargazer(code_lang_count[1:10, ], summary=FALSE, rownames=FALSE)

ggplot(code_lang_count[1:10, ], aes(x=reorder(V1, -N), y=N))+geom_bar(stat="identity", fill=rgb(44, 60, 67, maxColorValue = 256))+
  ggtitle("Coding Languages (Top 10)")+ xlab("Coding Language")+ylab("Count")+theme_classic()+
  theme(plot.title = element_text(size=28), axis.title.x = (element_text(size=15)), axis.title.y = (element_text(size=15)), axis.text=(element_text(size=12)), text=element_text(family="Franklin Gothic Book"))

#### 3: descriptions, processing

#any way to identify language? definitely descriptions in other languages
#UPDATE: cld2 seems to be the best at detecting english, cld3 seems to be better at detecting other language (definitely not perfect)
raw_data$lang_cld3<-lapply(raw_data$description, cld3::detect_language)
#raw_data$lang_textcat<-lapply(raw_data$description, textcat)
raw_data$lang_cld2<-lapply(raw_data$description, cld2::detect_language)


#filter out english entries (removes about 60%, kept 5742/10000, 1084/10000 are non-english, the rest have no description)
raw_data$lang_cld2<-paste(raw_data$lang_cld2, collapse="/") %>% str_split("/")
raw_data$lang_cld3<-paste(raw_data$lang_cld3, collapse="/") %>% str_split("/")

raw_desc<-raw_data %>% filter.(is.na(description)==FALSE) 

cld2_total<-as.data.table(table(raw_desc$lang_cld2)) %>% arrange.(desc(N))
cld3_total<-as.data.table(table(raw_desc$lang_cld3)) %>% arrange.(desc(N))
total_lang<-left_join.(cld2_total, cld3_total, "V1") %>% arrange.(desc(N))
colnames(total_lang)<-c("Language", "cld3", "cld2")

stargazer(total_lang[1:8, ], summary=FALSE)
#viz
ggplot(total_lang[1:6,], aes(x=Language, y=cld2))+geom_bar(position="dodge", stat="identity")


english_description<- raw_data %>% filter.(lang_cld2=="en"|lang_cld3=="en") %>% select.("description", "primarylanguage") 

non_english_description<-anti_join.(raw_data, english_description, by="description") %>% 
  filter.(is.na(description)==FALSE) %>% 
  select.("description", "primarylanguage", "lang_cld3", "lang_cld2")

cld3_count<-as.data.table(table(non_english_description$lang_cld3)) %>% arrange.(desc(N))
cld2_count<-as.data.table(table(non_english_description$lang_cld2)) %>% arrange.(desc(N))

final_lang_count<-left_join.(cld2_count, cld3_count, "V1")


# maybe do a coding language breakdown for these too?
eng_code_lang_count<-as.data.table(table(english_description$primarylanguage)) %>% arrange.(desc(N))
head(eng_code_lang_count, n=10)

non_eng_code_lang_count<-as.data.table(table(non_english_description$primarylanguage)) %>% arrange.(desc(N))
head(non_eng_code_lang_count)

#clean strings: remove emojis
processed_description<-english_description %>% mutate.(description=tolower(description)) 
#str_replace_all(processed_description$description, '[^\x01-\x7F]', '')

processed_description$description<-iconv(processed_description$description, "UTF-8", "ASCII", sub = "")

# quick tally of word counts in each description (english)







#### 4: dictionary approach to classification (from 02_nested_dict_classification)
## based on summary type 
utility <- software_types %>% filter(summary_type == "Utility")
utility <- paste(c("\\b(?i)(zcx", utility$terms, "zcx)\\b"), collapse = "|")

programming<-software_types %>% filter.(summary_type=="Programming")
programming<-paste(c("\\b(?i)(zcx", programming$terms, "zcx)\\b"), collapse = "|")

#programming<-programming %>% filter.(sub_type=="R"|sub_type=="Python"|sub_type=="SPSS"|sub_type=="SAS"|sub_type=="Scala"|sub_type=="Julia") %>% mutate.(main_type="Statistical")
programming[, main_type:=ifelse(sub_type=="R"|sub_type=="Python"|sub_type=="SPSS"|sub_type=="SAS"|sub_type=="Scala"|sub_type=="Julia", "Statistical Software", NA)]

## based on main_type 
blockchain <- software_types %>% filter(main_type == "Blockchain")
blockchain <- paste(c("\\b(?i)(zcx", blockchain$terms, "zcx)\\b"), collapse = "|")

communications <- software_types %>% filter(main_type == "Communications")
communications <- paste(c("\\b(?i)(zcx", communications$terms, "zcx)\\b"), collapse = "|")


## based on sub_type
front_end<-software_types %>% filter(sub_type=="Front-Ends")
database_servers <- software_types %>% filter(sub_type == "Database Engines/Servers")
#database_servers <- paste(c("\\b(?i)(zcx", database_servers$terms, "zcx)\\b"), collapse = "|")

cryptocurrency<-software_types %>% filter(sub_type=="Crytocurrency")



#utility-security
anti_spam<-software_types %>% filter(sub_type=="Anti-Spam")
anti_virus<-software_types %>% filter(sub_type=="Anti-Virus")
anti_malware<-software_types %>% filter(sub_type=="Anti-Malware")
password_manager<-software_types %>% filter(sub_type=="Password Manager")
cryptography<-software_types %>% filter(sub_type=="Cryptography")


# development space (so you don't have to do refresh the csv with each update) ###########################################

# it gets annoying to go back to that csv each time so you can just add words to the string and then rerun your function 

#application, database
database_servers <- paste(c("\\b(?i)(zcx", database_servers$terms, 
                            "database management|solarwinds database|dbvisualizer|manageengine|oracle rdbms
                            |ibm db2|microsoft sql|sap sybase ase|teradata|adabas|mysql|filemaker|microsoft access
                            |informix|sqlite|postgresql|amazonrds|mongodb|rdeis|couchdb|neo4j|couchbase|orientdb
                            |phpmyadmin|sql developer|seqel pro|robomongo|hadoop hdfs|cloudera|mariadb
                            |informix dynamic server|altibase|xampp|appserv|firebird|uniform server",
                            "zcx)\\b"), collapse = "|")
front_end<-paste(c("\\b(?i)(zcx",
                   "ta-lib|squirrel|ladp admin|ermaster|talend open studio|talend|hypersqul|pgweb|ucanaccess|
                   vertigoserv|geotools|redash|omi|metabase|nubuilderforte|racktables|html5 boilerplate|compactview",
                   "zcx)\\b"), collapse = "|")

#application, blockchain
cryptocurrency<-paste(c("\\b(?i)(zcx", cryptocurrency$terms, 
                        "miner|multipoolminer|easyminer|corda|gekko|zeronet|monero|xrp ledger", "zcx)\\b"), collapse="|")
#utility, security
anti_spam<-paste(c("\\b(?i)(zcx", 
                   "scrollout f1|anti-spam smtp proxy server|gophish|opendkim|mailcleaner|copfilter|cidram", "zcx)\\b"), collapse="|")

anti_virus<-paste(c("\\b(?i)(zcx",  
                    "clamwin|dex2jar|robolinux|clam sentinel|antivirus live cd|
                    c-icap|after death antivirus|virus effect remover|no autorun", "zcx)\\b"), collapse="|")

anti_malware<-paste(c("\\b(?i)(zcx",  
                      "clamwin|hijackthis|cuckoo sandbox|detekt|robolinux|
                      clam sentinel|santa|remnux|honeydrive|rinzler usb cleaner", "zcx)\\b"), collapse="|")
password_manager<-paste(c("\\b(?i)(zcx", 
                          "pac manager|password tech|password safe", "zcx)\\b"), collapse="|")
cryptography<-paste(c("\\b(?i)(zcx", 
                      "winscp|mimikatz|waircut|shadowsocks|veracrypt|beecrypt|jsign|freeotef", "zcx)\\b"), collapse="|")


# and then if you want to output your updated string and paste it back to the csv you can do it with this 
vector_to_unnest <- "database mangement|databases"
vector_to_unnest <- data.frame(unlist(strsplit(vector_to_unnest, "\\|")))
colnames(vector_to_unnest) <- "terms"
write_csv(vector_to_unnest, str_c(path_for_data, "terms_to_update.csv"))


# classification #########################################################################################################

description_classified <- processed_description %>% 
  dt_mutate(blockchain = ifelse(test = str_detect(string = description, pattern = blockchain), 1, 0)) %>%
  dt_mutate(communications = ifelse(test = str_detect(string = description, pattern = communications), 1, 0)) %>% 
  dt_mutate(database_servers=ifelse(test=str_detect(string=description, pattern=database_servers), 1, 0)) %>% 
  dt_mutate(cryptocurrency=ifelse(test=str_detect(string=description, pattern=cryptocurrency), 1, 0)) %>% 
  dt_mutate(anti_malware=ifelse(test=str_detect(string=description, pattern=anti_malware), 1, 0)) %>% 
  dt_mutate(anti_spam=ifelse(test=str_detect(string=description, pattern=anti_spam), 1, 0)) %>% 
  dt_mutate(anti_virus=ifelse(test=str_detect(string=description, pattern=anti_virus), 1, 0)) %>% 
  dt_mutate(cryptography=ifelse(test=str_detect(string=description, pattern=cryptography), 1, 0)) %>% 
  dt_mutate(password_manager=ifelse(test=str_detect(string=description, pattern=password_manager), 1, 0)) %>% 
  dt_mutate(front_end=ifelse(test=str_detect(string=description, pattern=front_end), 1, 0))
