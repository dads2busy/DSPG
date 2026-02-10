get_db_conn <-
  function(db_name = "sdad",
           db_host = "postgis1",
           db_port = "5432",
           db_user = Sys.getenv("db_usr"), # requires you to setup environmental vars (above)
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

get_rows <- function(cols, fips) {
  sql <- paste0("SELECT ",
                cols,
                " WHERE fips_code='", fips, "'")
  con <- get_db_conn()
  rows <-
    RPostgreSQL::dbGetQuery(con, statement = sql)
  RPostgreSQL::dbDisconnect(con)
  data.table::setDT(rows)
}
