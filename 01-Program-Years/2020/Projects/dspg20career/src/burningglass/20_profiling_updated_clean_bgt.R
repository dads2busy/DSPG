library(maditr)
library(tidyverse)
library(data.table)
library(stringr)
library(stringi)
library(mosaic)
library(dplyr)
library(readr)
library(gt)
library(lubridate)

source("~/git/DSPG2020/career/src/burningglass/20_updated_clean_bgt.R")

bg_cleaned <- get_bgt_data_for_state(states = c("VA", "DC", "MD"))

bg_covariates <- as.data.table(bg_cleaned$bg_covariates)
bg_job <- as.data.table(bg_cleaned$bg_job)
all_majors <- as.data.table(bg_cleaned$all_majors)
all_skills <- as.data.table(bg_cleaned$all_skills)
highest_degree_majors <- as.data.table(bg_cleaned$highest_degree_majors)
top_skills <- as.data.table(bg_cleaned$top_skills)

write.csv(bg_covariates, file= "~/git/DSPG2020/career/data/bg_covariates_vadcmd.csv", row.names = F)
write.csv(bg_job, file= "~/git/DSPG2020/career/data/bg_job_vadcmd.csv", row.names = F)
write.csv(all_majors, file= "~/git/DSPG2020/career/data/all_majors_vadcmd.csv", row.names = F)
write.csv(all_skills, file= "~/git/DSPG2020/career/data/all_skills_vadcmd.csv", row.names = F)
write.csv(highest_degree_majors, file= "~/git/DSPG2020/career/data/highest_degree_majors_vadcmd.csv", row.names = F)
write.csv(top_skills, file= "~/git/DSPG2020/career/data/top_skills_vadcmd.csv", row.names = F)

data_profiling <- function(df){
  nrow = nrow(df)
  summary_table <- tibble(var = names(df),
                          variable_type = map_chr(.x = df, .f = function(col) class(x = col)),                 
                          num_unique = map_int(.x = df, .f = function(col) length(x = unique(x = col))),
                          num_missing = map_int(.x = df, .f = function(col) sum(x = is.na(x = col)))) %>%
    mutate(perc_missing = round(x = 100 * num_missing / nrow, digits = 2L))
  
  return(summary_table)
}

bg_covariates %>% group_by(gender) %>% count()
bg_covariates %>% group_by(veteran) %>% count()
bg_covariates %>% group_by(zipcode) %>% count() %>% arrange(-n)
pers_cleaned %>% group_by(highest_degree_bydegreetype) %>% count() %>% arrange(-n)
pers_cleaned %>% group_by(highest_degree_bydegreelevel) %>% count() %>% arrange(-n)

data_profiling(bg_covariates)
data_profiling(pers_cleaned)

