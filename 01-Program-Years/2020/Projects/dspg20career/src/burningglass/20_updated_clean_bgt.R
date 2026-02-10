# New clean_bg.R script to call new clean_bgt function.
# Goals:
#     1. Add functionality to select geographic subset in function call
#     2. Put veterans and nonveterans in the same dataset with an indicator variable
#     3. Fold in the many new explanatory variables into demographic files
#     4. Add education state
#     5. Add imputed job zone state for missing

get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"),
           db_pass = Sys.getenv("db_pwd")) {
    RPostgreSQL::dbConnect(
      drv = RPostgreSQL::PostgreSQL(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_pass
    )
  }

## BEGIN FUNCTION --------------------------------------------
get_bgt_data_for_state <- function(states = c("VA", "DC", "MD")) {
  
    onet <- fread("~/git/DSPG2020/career/src/burningglass/onet_avg_zone.csv")
  forprofit <- fread("~/git/DSPG2020/career/src/burningglass/ipeds_forprofit.csv") %>% pull(UnitID)
  hbcu <- fread("~/git/DSPG2020/career/src/burningglass/ipeds_hbcu.csv") %>% pull(UnitID)
  tribal <- fread("~/git/DSPG2020/career/src/burningglass/ipeds_tribal.csv") %>% pull(UnitID)
  uszips <- fread("~/git/DSPG2020/career/src/burningglass/uszips.csv")
  irr <- readxl::read_xlsx("~/git/DSPG2020/career/src/burningglass/IRR_2000_2010.xlsx", 
                           sheet = 2)

## PULL ZIPCODE FOR STATES IN STATES
zips <- uszips %>% filter(state_id %in% states) %>% pull(zip)

con <- get_db_conn()

## PULL ENTIRE PERS TABLE
pers <- DBI::dbGetQuery(con, "SELECT * FROM bgt_res.pers")

## PULL IDS OF PERS IN ZIPCODES, PULL FROM OTHER TABLES BASED ON IDS
ids <- pers %>% filter(zipcode %in% zips) %>% pull(id)

#job <- DBI::dbGetQuery(con, paste("SELECT * FROM bgt_res.job WHERE id IN (", paste(ids, collapse = ", "), ") AND onet LIKE '55-%'"))
#vet_ids <- job %>% filter(!is.na(onet) &  !is.na(startdate) & !is.na(enddate)) %>% select(id) %>% distinct() %>% pull(id)
#nonvet_ids <- pers %>% filter(zipcode %in% zips) %>% filter(!(id %in% vet_ids)) %>% sample(100000, replace = FALSE) %>% pull(id)
#ids <- c(vet_ids, nonvet_ids)
job <- DBI::dbGetQuery(con, paste("SELECT * FROM bgt_res.job WHERE id IN (", paste(ids, collapse = ", "), ")")) %>% filter(!is.na(onet) &  !is.na(startdate) & !is.na(enddate))

ed <- DBI::dbGetQuery(con, paste("SELECT * FROM bgt_res.ed WHERE id IN (", paste(ids, collapse = ", "), ")"))
ed <-ed[,1:12]

job <- DBI::dbGetQuery(con, paste("SELECT * FROM bgt_res.job WHERE id IN (", paste(ids, collapse = ", "), ")"))
job <- job[, 1:9]

skill <- DBI::dbGetQuery(con, paste("SELECT * FROM bgt_res.skill WHERE id IN (", paste(ids, collapse = ", "), ")"))
#skill <- skill[,-1]

DBI::dbDisconnect(con)

message("Finished pulling BG tables")

#bg <- list(job, ed, pers, onet, skill)
#return(bg)
#}

#clean_bg <- function(job = job, ed = ed, pers = pers, onet = onet){
  #convert all dataframes to data table
  job <- as.data.table(job) %>% mutate(start_year = year(startdate), end_year = year(enddate)) %>% filter(start_year >= 1951) %>% filter(end_year >= 1951) %>% 
    filter(!is.na(onet) &  !is.na(start_year) & !is.na(end_year))
  ed <- as.data.table(ed)
  pers <- as.data.table(pers)
  skill <- as.data.table(skill)
  onet <- as.data.table(onet)
  
## FIRST CLEANING TABLES FOR DEMOGRAPHIC VARIABLES/COVARIATES
## (VETERAN, GENDER, ZIPCODE, HIGHEST DEGREE EARNED degreetype, HIGHEST DEGREE EARNED degreelevel, RURALITY INDEX, INSTITUTION for profit/not for profit, YEARS IN MILITARY, YEAR OF MILITARY ENLIST, YEAR OF MILITARY EXIT, HIGHEST MILITARY O*NET, YEAR ENTERED THE WORKFORCE)

# VETERAN, GENDER, ZIPCODE
  military <- "55-[0-9][0-9][0-9][0-9].[0-9][0-9]"
  #extract unique military id
  military_ids <- job %>% 
    filter(str_detect(job$onet, military))%>%
    distinct(id)%>%
    pull(id)
  pers_cleaned <- pers %>% filter(id %in% ids) %>%
    mutate(veteran = if_else(id %in% military_ids, "veteran", "not veteran"))%>%
    select(id, veteran, gender, zipcode) %>% distinct()
  
# HIGHEST DEGREE EARNED degreelevel, INSTITUTION for profit/not for profit
  ed$degreelevel <- as.numeric(ed$degreelevel)
  ed_cleaned <- ed %>% select(id, degreelevel, ipeds_unit_id, majorcipcode) %>%
    mutate(degreetype = case_when(
      degreelevel == 12 ~ "highschool",
      degreelevel == 13 ~ "GED/diploma",
      degreelevel == 14 ~ "associates",
      degreelevel == 16 ~ "bachelors",
      degreelevel == 17 ~ "post-bacc",
      degreelevel == 18 ~ "masters",
      degreelevel == 21 ~ "phd"),
      school_forprofit = ifelse(ipeds_unit_id %in% forprofit, 1, 0),
      school_hbcu = ifelse(ipeds_unit_id %in% hbcu, 1, 0),
      school_tribal = ifelse(ipeds_unit_id %in% tribal, 1, 0)) %>%
    mutate(major = str_extract_all(majorcipcode, "[0-9]{2}(?=\\.)")) %>% 
    group_by(id) %>%
    mutate(highest_degree = case_when(
      max(degreelevel, na.rm = TRUE) == 12 ~ "highschool",
      max(degreelevel, na.rm = TRUE) == 13 ~ "GED/diploma",
      max(degreelevel, na.rm = TRUE) == 14 ~ "associates",
      max(degreelevel, na.rm = TRUE) == 16 ~ "bachelors",
      max(degreelevel, na.rm = TRUE) == 17 ~ "post-bacc",
      max(degreelevel, na.rm = TRUE) == 18 ~ "masters",
      max(degreelevel, na.rm = TRUE) == 21 ~ "phd"),
      forprofit = ifelse(sum(school_forprofit) > 0, "for profit", "not for profit"),
      hbcu = ifelse(sum(school_hbcu) > 0, "hbcu", "not hbcu"),
      tribal = ifelse(sum(school_tribal) > 0, "tribal", "not tribal")) %>%
    select(id, degreelevel, highest_degree, forprofit, hbcu, tribal, major) #%>%
    #rename(highest_degree_bydegreelevel = highest_degree)

# RURALITY INDEX
  zips_by_county <- uszips %>% separate_rows(county_weights, sep = "[,]") %>% 
    mutate(FIPS2010 = str_extract(county_weights, "[0-9][0-9][0-9][0-9][0-9]")) %>%
    mutate(fips_weight = str_extract(county_weights, "(?<=:).*[^}]"))
  zips_by_county$FIPS2010 <- as.numeric(zips_by_county$FIPS2010)
  zips_by_county$fips_weight <- as.numeric(zips_by_county$fips_weight)
  
  zips_by_county$county_fips_all <- as.numeric(zips_by_county$county_fips_all)
  
  rural_zips_irr <- left_join(zips_by_county, irr, by = c("FIPS2010")) %>% 
    group_by(zip) %>% mutate(irr = weighted.mean(IRR2010, fips_weight, na.rm = TRUE)) %>% distinct(zip, irr)
  rural_zips_irr$zip <- as.character(rural_zips_irr$zip)
  
  pers_cleaned <- pers_cleaned %>% 
    mutate(zip = str_extract(zipcode, "^[0-9][0-9][0-9][0-9][0-9]")) %>%
    left_join(rural_zips_irr, by = c("zipcode" = "zip")) %>%
    left_join(ed_cleaned, by = "id") %>%
    select(id, veteran, gender, zipcode, irr, highest_degree, forprofit, hbcu, tribal)
  
# YEARS IN MILITARY, YEAR OF MILITARY ENLIST, YEAR OF MILITARY EXIT, HIGHEST MILITARY O*NET
officers <- c("55-10[0-9][0-9].[0-9][0-9]")
  
veteran_job <- job %>% filter(id %in% military_ids) %>% mutate(
    is_onet55 = if_else(str_detect(onet, "55-[0-9][0-9][0-9][0-9].[0-9][0-9]"), T, F),
    is_officer = if_else(str_detect(onet, "55-10[0-9][0-9].[0-9][0-9]"), T, F),
    duration = end_year - start_year + 1) %>% 
  filter(is_onet55 == TRUE) %>% 
    group_by(id) %>% mutate(
      year_military_enlist = min(start_year),
      year_military_exit = max(end_year),
      officer = ifelse(any(is_officer) == TRUE, "officer", "not officer"),
      end_onet55 = ifelse(end_year == year_military_exit, onet, NA),
      years_in_military = year_military_exit - year_military_enlist + 1) %>% 
  drop_na(end_onet55) %>%
  select(id, years_in_military, year_military_enlist, year_military_exit, officer, end_onet55)
  
# YEAR ENTERED THE WORKFORCE
year_entered_civilian_workforce <- job %>%
  left_join(veteran_job, by = "id") %>% distinct() %>%
  mutate(veteran = ifelse(id %in% military_ids, "veteran", "not veteran"),
  duration = end_year - start_year + 1) %>% group_by(id) %>%
  mutate(aligned_start = min(start_year)) %>% ungroup() %>% 
  mutate(out = ifelse(veteran == "veteran" & start_year < year_military_exit, 1, 0)) %>%
  group_by(id, out) %>%
  mutate(year_entered_civilian_workforce = min(start_year)) %>% group_by(id) %>%
  mutate(year_entered_civilian_workforce = max(start_year)) %>%
  select(id, aligned_start, year_entered_civilian_workforce)

career_before_military <- job %>% 
  left_join(veteran_job, by = "id") %>% distinct() %>%
  mutate(
    is_onet55 = if_else(str_detect(onet, "55-[0-9][0-9][0-9][0-9].[0-9][0-9]"), T, F),
    veteran = ifelse(id %in% military_ids, "veteran", "not veteran"),
         duration = end_year - start_year + 1) %>% filter(veteran == "veteran") %>%
  mutate(out = ifelse((is_onet55 == TRUE | start_year > year_military_enlist), 1, 0)) %>%
  group_by(id, out) %>%
  mutate(career_before_military = year_military_enlist - min(start_year)) %>% group_by(id) %>% mutate(career_before_military = max(career_before_military)) %>%
  select(id, career_before_military)
  
# JOINING

demographic <- pers_cleaned %>%
  left_join(veteran_job, by = "id") %>%
  left_join(year_entered_civilian_workforce, by = "id") %>%
  left_join(career_before_military, by = "id") %>% distinct()

message("Finished creating demographic table")
  
## SOME VARIABLES GET THEIR OWN TABLES
## (ALL MAJORS, HIGHEST DEGREE MAJORS, ALL SKILLS, TOP SKILLS, ALL INDUSRIES, TOP INDUSTRIES)
 
## ALL MAJORS 
  majors <- ed_cleaned %>% select(id, degreelevel, major) %>% unnest(major) %>%
    drop_na(major) %>% group_by(id) %>% 
    mutate(max_degree = ifelse(is.na(max(degreelevel)), 0, max(degreelevel))) %>%
    mutate(highest_degree_major = ifelse(max_degree == degreelevel, major, NA)) 
  #%>% mutate(count = 1) %>% group_by(id, major) %>% 
  #mutate(major_frequency = sum(count)) %>% ungroup(major) %>%
  #mutate(max_major_frequency = max(major_frequency)) %>%
  #mutate(most_frequent_major = ifelse(major_frequency == max_major_frequency, 
  #major, NA)) %>%
  
  all_majors <- majors %>%
    melt(id.vars = "id", measure.vars = c("major", "highest_degree_major")) %>%
    filter(variable == "major") %>% mutate(value_wide = 1) %>% 
    distinct() %>%
    pivot_wider(names_from = "value", values_from = "value_wide", values_fill = 0)

## HIGHEST DEGREE MAJORS 
  highest_degree_majors <- majors %>%
    melt(id.vars = "id", measure.vars = c("major", "highest_degree_major")) %>%
    filter(variable == "highest_degree_major") %>% mutate(value_wide = 1) %>% 
    distinct() %>%
    pivot_wider(names_from = "value", values_from = "value_wide", values_fill = 0)
  
## ALL SKILLS
  all_skill <- skill %>% select(id, clusterfamily) %>% mutate(value_wide = 1) %>%
    distinct() %>% pivot_wider(names_from = "clusterfamily", values_from = "value_wide", values_fill = 0)

## TOP SKILLS
  top_skill <- skill %>% select(id, clusterfamily) %>% drop_na(clusterfamily) %>%
    mutate(value_wide = 1) %>%
    group_by(id, clusterfamily) %>% mutate(skill_sum = sum(value_wide)) %>% 
    distinct() %>% ungroup(clusterfamily) %>% slice_max(skill_sum, n = 1) %>% 
    mutate(value_wide = 1) %>% pivot_wider(names_from = "clusterfamily", values_from = "value_wide", values_fill = 0)
  
message("Finished creating supplementary demographic tables")
  
## JOBS TABLE -----------------------------
  
  job_cleaned <- job

  #4. cleaning onet table
  onet_cleaned <- onet %>%
    select(onetsoc_code, title, new_zone)%>%
    rename("onet_title" = "title")
  
  #join all tbs
  bg_full <- job_cleaned %>%
    left_join(onet_cleaned, by = c("onet" = "onetsoc_code")) %>%
    mutate(is_onet55 = if_else(str_detect(onet, "55-[0-9][0-9][0-9][0-9].[0-9][0-9]"), T, F)) %>%
    mutate(onet_job_zone = if_else(is_onet55 == TRUE, 55, as.double(new_zone)))  #assign 55 to military jobs (ONET 55-)
  
  #5. clean joined table
  # clean job zone
  #table(is.na(bg_full$onet_job_zone))

    # Education dates 
    education_dates <- ed %>% 
    select(id, completiondateraw, degreelevel) %>% drop_na() %>% separate_rows(degreelevel, sep = "#") %>% mutate(degreeid = rownames(.))
  
  education_dates <- education_dates %>% separate_rows(completiondateraw, sep = "#") %>% mutate(date = parse_date_time(completiondateraw, c("Y", "mY", "my", "y", "d y", "d Y", "mdY", "mdy", "b y", "Y m"))) %>% mutate(clean = ifelse(is.na(date), "no", "yes")) %>% mutate(left = sum(is.na(date))) %>% filter(year(date) < 2020)
  
  education_dates$date <- as.Date(education_dates$date, format = "%Y-%m-%d")
  
  ## Change to format of jobs data, impute education duration
  education_dates <- education_dates %>%
    mutate(onet_job_zone = 6, end_year = year(date), startdate = case_when(
      degreelevel == 12 ~ date - years(2), # highschool, picking 2 years
      degreelevel == 13 ~ date - years(1),  # GED/diploma
      degreelevel == 14 ~ date - years(2),  # associates
      degreelevel == 16 ~ date - years(4),  # bachelors
      degreelevel == 17 ~ date - years(1),  # post-bac
      degreelevel == 18 ~ date - years(2),  # masters
      degreelevel == 21 ~ date - years(4)   #phd
    )) %>% mutate(start_year = year(startdate)) %>%
    select(id, onet_job_zone, end_year, start_year) %>%
    filter(start_year >= 1951) %>% filter(end_year >= 1951) %>%
    filter(start_year <= 2017)

  bg_full <- bg_full %>% select(id, onet_job_zone, end_year, start_year) %>% rbind(education_dates)
  
message("Finished making jobs table")
  
  #output table 3
  bg_job <- bg_full %>%
    select(id, onet_job_zone, start_year, end_year) %>%
    filter(!is.na(onet_job_zone) &  !is.na(start_year) & !is.na(end_year))
  bg_job_ids <- bg_job %>% distinct() %>% pull(id)
  
  bg_covariates <- demographic %>% filter(id %in% bg_job_ids) %>% distinct()
  all_majors <- all_majors %>% filter(id %in% bg_job_ids) %>% distinct()
  highest_degree_majors <- highest_degree_majors %>% filter(id %in% bg_job_ids) %>% distinct()
  all_skills <- all_skill %>% filter(id %in% bg_job_ids) %>% distinct()
  top_skills <- top_skill %>% filter(id %in% bg_job_ids) %>% distinct()

message("Finished putting all tables together")
  
  bg_cleaned <- list(bg_covariates, bg_job, all_majors, highest_degree_majors, all_skills, top_skills)
  names(bg_cleaned) <- c("bg_covariates", "bg_job", "all_majors", "highest_degree_majors", "all_skills", "top_skills")
  return(bg_cleaned)
}
