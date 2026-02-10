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

#uszips <- fread("~/git/DSPG2020/career/src/burningglass/uszips.csv")
#zips <- uszips %>% filter(state_id %in% c("DC", "MD", "VA")) %>% pull(zip)

con <- get_db_conn()

pers <- DBI::dbGetQuery(con, "SELECT * FROM bgt_res.pers")
ids <- pers %>% filter(zipcode %in% zips) %>% pull(id)

ed <- DBI::dbGetQuery(con, paste("SELECT * FROM bgt_res.ed WHERE id IN (", paste(ids, collapse = ", "), ")"))
ed <-ed[,1:12]

#job <- DBI::dbGetQuery(con, paste("SELECT * FROM bgt_res.job WHERE id IN (", paste(vet_ids, collapse = ", "), ")")) %>% filter(!is.na(onet) &  !is.na(startdate) & !is.na(enddate))
#job <- job[, 1:9]

DBI::dbDisconnect(con)

forprofit <- fread("~/git/DSPG2020/career/src/burningglass/ipeds_forprofit.csv") %>% pull(UnitID)
hbcu <- fread("~/git/DSPG2020/career/src/burningglass/ipeds_hbcu.csv") %>% pull(UnitID)
tribal <- fread("~/git/DSPG2020/career/src/burningglass/ipeds_tribal.csv") %>% pull(UnitID)

highest_degree_from_forprofit <- ed %>% separate_rows(degreelevel, sep = "#") %>% 
  select(id, degreelevel, ipeds_unit_id) %>% filter(ipeds_unit_id %in% forprofit) %>% group_by(id) %>% mutate(highest_degree_from_forprofit = case_when(
  max(degreelevel, na.rm = TRUE) == 12 ~ "highschool",
  max(degreelevel, na.rm = TRUE) == 13 ~ "GED/diploma",
  max(degreelevel, na.rm = TRUE) == 14 ~ "associates",
  max(degreelevel, na.rm = TRUE) == 16 ~ "bachelors",
  max(degreelevel, na.rm = TRUE) == 17 ~ "post-bacc",
  max(degreelevel, na.rm = TRUE) == 18 ~ "masters",
  max(degreelevel, na.rm = TRUE) == 21 ~ "phd",
  is.na(degreelevel) ~ "missing")) %>% distinct(id, highest_degree_from_forprofit)

highest_degree_from_hbcu <- ed %>% separate_rows(degreelevel, sep = "#") %>% 
  select(id, degreelevel, ipeds_unit_id) %>% filter(ipeds_unit_id %in% hbcu) %>% group_by(id) %>% mutate(highest_degree_from_hbcu = case_when(
    max(degreelevel, na.rm = TRUE) == 12 ~ "highschool",
    max(degreelevel, na.rm = TRUE) == 13 ~ "GED/diploma",
    max(degreelevel, na.rm = TRUE) == 14 ~ "associates",
    max(degreelevel, na.rm = TRUE) == 16 ~ "bachelors",
    max(degreelevel, na.rm = TRUE) == 17 ~ "post-bacc",
    max(degreelevel, na.rm = TRUE) == 18 ~ "masters",
    max(degreelevel, na.rm = TRUE) == 21 ~ "phd",
    is.na(degreelevel) ~ "missing")) %>% distinct(id, highest_degree_from_hbcu)


highest_degree_from_forprofit$id <- as.character(highest_degree_from_forprofit$id)
highest_degree_from_hbcu$id <- as.character(highest_degree_from_hbcu$id)