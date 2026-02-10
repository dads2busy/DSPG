# read in the Burning Glass resume data

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

list_db_schemas <- function(db_con) {
  result <- DBI::dbGetQuery(db_con, "select schema_name from information_schema.schemata")
  DBI::dbDisconnect(db_con)
  return(result)
}

list_schema_tables <- function(db_con, db_schema) {
  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_name FROM information_schema.tables
                                 WHERE table_schema='", db_schema, "'"))
  DBI::dbDisconnect(db_con)
  return(result)
}

list_table_columns <- function(db_con, db_schema, db_table) {
  result <- DBI::dbGetQuery(db_con, paste0("SELECT table_schema, table_name, column_name, data_type
                                 FROM information_schema.columns
                                 WHERE table_schema = '", db_schema, "'",
                                           " AND table_name = '", db_table, "'"))
  DBI::dbDisconnect(db_con)
  return(result)
}

# EXAMPLES ----
# List Schemas
con <- get_db_conn()
list_db_schemas(con)
# you should see bgt_res in this list

# List Tables in a Schema
con <- get_db_conn()
list_schema_tables(con, "bgt_res")

# List Columns in a Table
con <- get_db_conn()
list_table_columns(con, "bgt_res", "job")

# Get Some Data
con <- get_db_conn()
DBI::dbGetQuery(con, 'SELECT * FROM bgt_res.job LIMIT 20')

# Get subsetted data
cert <- DBI::dbGetQuery(con, "
                        SELECT * FROM bgt_res.cert
                        WHERE cert.id in (
                        SELECT ID FROM bgt_res.pers
                        WHERE pers.msa like '%47900%'
                        )"
)

ed <- DBI::dbGetQuery(con, "
                      SELECT * FROM bgt_res.ed
                      WHERE ed.id in (
                      SELECT ID FROM bgt_res.pers
                      WHERE pers.msa like '%47900%'
                      )"
)

job <- DBI::dbGetQuery(con, "
                       SELECT * FROM bgt_res.job
                       WHERE job.id in (
                       SELECT ID FROM bgt_res.pers
                       WHERE pers.msa like '%47900%'
                       )"
)

pers <- DBI::dbGetQuery(con, "
                        SELECT * FROM bgt_res.pers
                        WHERE pers.msa like '%47900%'"
)

skill <- DBI::dbGetQuery(con, "
                         SELECT * FROM bgt_res.skill
                         WHERE skill.id in (
                         SELECT ID FROM bgt_res.pers
                         WHERE pers.msa like '%47900%'
                         )"
)


DBI::dbDisconnect(con)

