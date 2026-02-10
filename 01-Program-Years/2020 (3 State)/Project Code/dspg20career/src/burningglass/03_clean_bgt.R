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

con <- get_db_conn()

ed <- DBI::dbGetQuery(con, "SELECT *

                      FROM bgt_res.ed A
                      JOIN bgt_res.id_msa_dc B
                      ON A.id = B.id"
)
ed <-ed[,1:12]

job <- DBI::dbGetQuery(con, "SELECT *
FROM bgt_res.job A
JOIN bgt_res.id_msa_dc B
ON A.id = B.id"
)
job <- job[, 1:9]

pers <- DBI::dbGetQuery(con, "SELECT *
                        FROM bgt_res.pers A
                        JOIN bgt_res.id_msa_dc B
                        ON A.id = B.id"
)
pers <- pers[,1:9]

onet <- DBI::dbGetQuery(con, "SELECT * FROM onet.job_zones")

DBI::dbDisconnect(con)

clean_bg <- function(job = job, ed = ed, pers = pers, onet = onet){
  #convert all dataframes to data table
  job <- as.data.table(job)
  ed <- as.data.table(ed)
  pers <- as.data.table(pers)
  onet <- as.data.table(onet)
  
  #cleaning before joining
  #1. cleaning ed table
  #create identifier for veterans
  military <- "55-[0-9][0-9][0-9][0-9].[0-9][0-9]"
  #extract unique military id
  military_ids <- job %>% 
    filter(str_detect(job$onet, military))%>%
    distinct(id)%>%
    pull(id)
  pers_cleaned <- pers%>%
    mutate(veteran =if_else(id %in% military_ids, "veteran", "not veteran"))%>%
    select(id, gender, noofjobs, veteran)
  
  #2. cleaning job table
  job_onet <- job %>%
    filter(!is.na(onet) & !is.na(startdate) & !is.na(enddate))

  job_missing_onet <- job%>%
    filter(!is.na(startdate) & !is.na(enddate))%>%
    group_by(id)%>%
    dt_mutate(enter = min(startdate))%>%
    filter(startdate == enter)%>%
    filter(is.na(onet))%>%
    select(-enter)
  
  job_cleaned <- rbind(job_onet,job_missing_onet)
  table(is.na(job_cleaned$onet)) #missing onet code
  
  job_cleaned <- job_cleaned%>%
    mutate(start_year = year(startdate), end_year = year(enddate), start_month = month(startdate), end_month = month(enddate), start_day = day(startdate), end_day = day(enddate))%>%
    mutate(job_duration_day = as.numeric(enddate- startdate))%>%
    mutate(enddate = if_else(enddate == "1900-01-01", startdate, enddate))%>%  #if missing enddate, system defaul to 1900-01-01, we recoded enddate = startdate
    mutate(end_year = year(enddate))%>%
    mutate(tenure = if_else(job_duration_day >= 365, "tenure", "not tenure"))%>%
    filter(job_duration_day >= 0)%>%
    group_by(id)%>%
    mutate(date_enter_job_market = min(startdate))
  

 # round((nrow(job)-nrow(job_cleaned))/nrow(job_cleaned) *100, digits = 2)
  
  #3. cleaning ed table
  ed_rename <- ed%>%
    filter(!is.na(degreetype))%>%
    mutate(degree_certificate = if_else(str_detect(string = degreetype, 
                                                   pattern = "\\b(?i)(certificate|certificate)\\b"),T,F))%>%
    mutate(degree_somehs = if_else(str_detect(string = degreetype, 
                                              pattern = "\\b(?i)(10|11|9)\\b"), T,F))%>%
    mutate(degree_hs = if_else(str_detect(string = degreetype, 
                                          pattern = "\\b(?i)(12|High School|ged)\\b"), T,F))%>%
    mutate(degree_associate = if_else(str_detect(string = degreetype, 
                                                 pattern = "\\b(?i)(Associate|associate)\\b"), T,F))%>%
    mutate(degree_bachelor = if_else(str_detect(string = degreetype, 
                                                pattern = "\\b(?i)(Bachelor|bachelor|Bachelors|BS|bs|B.S|BA|B.A|ba|AA|A.A|Undergraduate|undergraduate|postgraduate|Post-Graduate|post-graduate)\\b"), T,F))%>%
    mutate(degree_master = if_else(str_detect(string = degreetype, 
                                              pattern = "\\b(?i)(master|Master|MBA|M.S|MS|MD)\\b"), T,F))%>%
    mutate(degree_doctor = if_else(str_detect(string = degreetype, 
                                              pattern = "\\b(?i)(phd|Ph.D|postdoc|J.D|JD)\\b"), T,F))
  
  #duplicated degree
  ed_rename_dup <- ed_rename%>%
    group_by(id)%>%
    mutate(number_degree = n())%>%
    filter(number_degree > 1)%>%
    as.data.table()%>%
    mutate(degree_highest = if_else(degree_doctor == T, "doctor", 
                                    if_else(degree_master == T, "master",
                                            if_else(degree_bachelor == T, "bachelor", 
                                                    if_else(degree_associate == T, "associate", 
                                                            if_else(degree_hs == T, "highschool",
                                                                    if_else(degree_somehs == T, "somehs", 
                                                                            if_else(degree_certificate ==T, "certificate", 
                                                                                    "others"))))))))
  
  ed_rename_dup <- ed_rename_dup%>%
    select(id, degree_highest)%>%
    tibble::rowid_to_column()%>%
    mutate(dup =T)%>%
    pivot_wider(names_from = degree_highest, values_from = dup)%>%
    select(-rowid)
  
  ed_rename_dup <- replace(ed_rename_dup, is.na(ed_rename_dup), FALSE)
  
  ed_rename_dup <- ed_rename_dup %>%
    arrange(id, -doctor, -master, -bachelor, -associate, -highschool, -somehs, -certificate, -others) %>%
    group_by(id)%>%
    filter(row_number()==1)%>%
    mutate(degree_highest = c("certificate", "somehs", "highschool", "associate", "bachelor","master", "doctor")[which.max(x = c(certificate, somehs, highschool, associate, bachelor, master, doctor))]) %>%
    select(id, degree_highest)%>%
    as.data.table()
  
  ## users with no duplicated degree
  ed_rename_no_dup <- ed_rename%>%
    group_by(id)%>%
    mutate(number_degree = n())%>%
    filter(number_degree == 1)%>%
    filter(!degree_bachelor)%>%
    as.data.table()%>%
    mutate(degree_highest = if_else(degree_doctor == T, "doctor", 
                                    if_else(degree_master == T, "master",
                                            if_else(degree_bachelor == T, "bachelor", 
                                                    if_else(degree_associate == T, "associate", 
                                                            if_else(degree_hs == T, "highschool",
                                                                    if_else(degree_somehs == T, "somehs", 
                                                                            if_else(degree_certificate ==T, "certificate", 
                                                                                    "others"))))))))%>%
    select(id, degree_highest)
  
  ##join the two table: ed_rename_no_dup, ed_rename_dup
  list = list(ed_rename_no_dup, ed_rename_dup)
  ed_cleaned <- rbindlist(list)
  
  #4. cleaning onet table
  onet_cleaned <- onet %>%
    select(onetsoc_code, title, job_zone)%>%
    rename("onet_title" = "title", "onet_job_zone" = "job_zone")
  
  #join all tbs
  bg_full <- pers_cleaned%>%
    left_join(ed_cleaned, by="id")%>%  #one person would only have one highest degree
    left_join(job_cleaned, by = "id")%>% #one person might have multiple job listed
    left_join(onet_cleaned, by = c("onet" = "onetsoc_code"))
  
  #5. clean joined table
  # clean job zone
  table(is.na(bg_full$onet_job_zone))
  bg_full_cleaned <- bg_full%>%
    mutate(is_onet55 = if_else(str_detect(onet, "55-[0-9][0-9][0-9][0-9].[0-9][0-9]"), T, F))%>%
    mutate(onet_job_zone = if_else(is_onet55, 55, onet_job_zone))%>%  #assign 55 to military jobs (ONET 55-)
    mutate(onet_job_zone = if_else(is.na(onet_job_zone), 0, onet_job_zone)) #assign job zone=0 to jobs that are missing job zones
  
  table(is.na(bg_full_cleaned$onet_job_zone)) 
  
  # output table 1
  bg_all_demographic <- bg_full_cleaned%>%
    select(id, gender, veteran, degree_highest, date_enter_job_market)
  bg_all_demographic <- bg_all_demographic[!duplicated(bg_all_demographic$id), ]
  
  # output table 2
  bg_vet_demographic <- bg_full_cleaned%>%
    filter(veteran == "veteran")%>%
    select(id, gender, degree_highest, date_enter_job_market)
  bg_vet_demographic <- bg_vet_demographic[!duplicated(bg_vet_demographic$id), ]
  
  #output table 3
  bg_all_job <- bg_full_cleaned %>%
    select(id, onet, is_onet55, noofjobs,sector, startdate, enddate, start_year, end_year, start_month, end_month, start_day, end_day, onet_title, onet_job_zone, job_duration_day, tenure, date_enter_job_market)%>%
    filter(!is.na(onet) &  !is.na(startdate) & !is.na(enddate))
  
  #output table 4
  bg_vet_job <- bg_full_cleaned %>%
    filter(veteran == "veteran")%>%
    select(id, onet,  is_onet55, noofjobs,sector, startdate, enddate,  start_year, end_year, start_month, end_month, start_day, end_day, onet_title, onet_job_zone, job_duration_day, tenure, date_enter_job_market)%>%
    filter(!is.na(onet)  &  !is.na(startdate) & !is.na(enddate))
  
  
  bg_cleaned <- list(bg_all_demographic, bg_vet_demographic, bg_all_job, bg_vet_job)
  names(bg_cleaned) <- c("bg_all_demographic", "bg_vet_demographic", "bg_all_job", "bg_vet_job")
  return(bg_cleaned)
}

