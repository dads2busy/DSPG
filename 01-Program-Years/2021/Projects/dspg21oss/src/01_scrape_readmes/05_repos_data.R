
# this file finds the readme data, repo stats data, and the original repos/commits data together
# note that we have two different samples we create in this file 
# the first is our 157k sample which is the final sample 
# (it includes all of the data we scraped that is above 349 commits)
# the other sample is contains more readme and repo_stats but the repos have fewer than 349 commits
# we will eventually collect all of the readme and repo_stats data but we ignore this for now

rm(list=ls())
library("tidyverse")
library("dplyr")
library("readr")
library("RPostgreSQL")


# readme data 
setwd("/project/class/bii_sdad_dspg/uva_2021/dspg21oss/")
readme_raw_data <- read_csv("oss_readme_data_071521.csv") %>% 
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
                          FROM gh_2007_2020.repos WHERE commits > 10;")
repos_table_349 <- dbGetQuery(conn, "SELECT slug, description, primarylanguage as language, commits 
                          FROM gh_2007_2020.repos WHERE commits > 349;")
dbDisconnect(conn)

# repo_stats data 
setwd("/project/class/bii_sdad_dspg/uva_2021/dspg21oss/")
repo_stats <- read_csv("repo_stats_0714.csv")

readme_404_removed <- readme_raw_data %>% 
  filter(readme_text != "404 ERROR - NO README")

repos_table_valid_desc <- repos_table %>% 
  filter(!is.na(description))

repos_table_valid_desc_349 <- repos_table_349 %>% 
  filter(!is.na(description))

combined <- repo_stats %>%
  left_join(readme_404_removed, by = "slug") %>% 
  left_join(repos_table_valid_desc , by = "slug") %>% 
  select(slug, description, readme_text, language, 
         topics, commits, forks, stars, watchers) %>%
  rename(readme = readme_text) %>% 
  arrange(-stars) 

combined_alt <- repo_stats %>%
  left_join(readme_404_removed, by = "slug") %>% 
  inner_join(repos_table_valid_desc_349 , by = "slug") %>% 
  select(slug, description, readme_text, language, 
         topics, commits, forks, stars, watchers) %>%
  rename(readme = readme_text) %>% 
  arrange(-stars) 

157538 / 10288063
358600 / 10288063

# repos/commits data 
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
dbWriteTable(conn, c("gh_2007_2020", "repos_subset_final"), combined_alt, row.names = FALSE)
#dbWriteTable(conn, c("gh_2007_2020", "repos_subset_157k"), combined_alt, row.names = FALSE)
#dbWriteTable(conn, c("gh_2007_2020", "repos_subset_358k"), combined, row.names = FALSE)
dbDisconnect(conn)

setwd("/project/class/bii_sdad_dspg/uva_2021/dspg21oss/")
write_csv(combined_alt, "github_repos_157k.csv")
write_csv(combined, "github_repos_358k.csv")

####

repos_noreadmes <- readme_raw_data %>% 
  filter(readme_text == "404 ERROR - NO README") %>% 
  select(slug, readme_text) %>% 
  mutate(missing_readme = 1)

# repos/commits data 
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
dbWriteTable(conn, c("gh_2007_2020", "repos_noreadmes"), repos_noreadmes, row.names = FALSE)
dbDisconnect(conn)

#### 

repos_above_349 <- repos_table %>% 
  filter(commits > 349)

repo_stats %>% 
  repos_above_349
  

# repos that dont exist (comparing repos and repo_stats)





