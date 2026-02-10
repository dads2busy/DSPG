# LOAD LIBRARIES
library(dplyr)
library(ggplot2)
library(purrr)
library(sf)  
library(venn)
library(ggthemes)
library(DataExplorer)
library(maditr)
library(RPostgreSQL)
library(data.table)

# LOAD FUNCTIONS
functions <- list.files("src/functions/", full.names = TRUE)
for (f in functions) source(f)

# SET COLUMNS
cols_usda_curr_tax <- readLines("src/settings/cols_usda_curr_tax.txt", n = 1)

# GET STATE FIPS
state_abbrevs <- c("VA", "WA", "AK", "IA", "ID", "MI", "MN", "NY", "OK", "OR", "CA", "CO", "GA", "NC", "OH", "TX")
state_fips <- get_state_fips(state_abbrevs)


for (sf in state_fips$state_code[13:16]) {
  # get name of current state
  current_state <- 
    state_fips %>% 
    filter(state_code == sf) %>% 
    select(state_name, state)
  print(paste("Working on", current_state$state_name))
  
  # create initial empty list
  completeness_list <- list()
  
  # get distinct (for this dataset) county-level fips for a state
  sql <-
    paste0(
      "select distinct fips_code from corelogic_usda.corelogic_usda_current_tax_2020_06_27 where fips_code like '",
      sf,
      "%'"
    )
  # get rows from db
  state_county_fips <- get_rows(sql)
  
  for (f in state_county_fips$fips_code) {
    completeness_list <- get_completeness(cols_usda_curr_tax, f)
    # add to list
    # completeness_list <- c(completeness_list, list(completeness))
  }
  
  assign(paste0("completeness_", current_state$state), rbindlist(completeness_list))
  
  fwrite(get(paste0("completeness_", current_state$state)), paste0("data/profiling/", paste0("completeness_", current_state$state, ".csv")))
}


fs <- list.files("data/profiling/", pattern = "*.csv", full.names = T)
fs_ls <- list()
for (f in fs) {
  dt <- fread(f, colClasses = c("character", "character", "integer", "integer", "integer", "numeric", "integer"))
  fs_ls <- c(fs_ls, list(dt))
}
fs_dt <- rbindlist(fs_ls)

con <- get_db_conn()
dbWriteTable(con, c("corelogic_usda", "corelogic_usda_current_tax_2020_06_27_completeness"), fs_dt, row.names = F, overwrite = T)
dbDisconnect(con)




