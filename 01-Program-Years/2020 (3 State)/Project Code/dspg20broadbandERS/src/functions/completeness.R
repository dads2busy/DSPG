check_calc <- function(vec) {
  blanks <- 0L
  true_na <- 0L
  written_na <- 0L
  len <- length(x = vec)
  for (elem in vec) {
    if (is.na(x = elem)) {
      true_na <- true_na + 1L
    } else if (elem == "na") {
      written_na <- written_na + 1L
    } else if (elem == "") {
      blanks <- blanks + 1L
    }
  }
  percent_complete <- round(((len - (blanks + true_na + written_na)) / len) * 100, 2)
  unique_values <- length(unique(vec))
  tibble(blanks = blanks,
         true_na = true_na,
         written_na = written_na,
         percent_complete = percent_complete,
         unique_values = unique_values)
}

check_complete <- function(df) {
  data.table::setDT(
  map_df(.x = df, .f = check_calc) %>%
    mutate(column = colnames(df)) %>%
    select(column, blanks, true_na, written_na, percent_complete, unique_values)
  )
}

get_completeness <- function(cols_usda_curr_tax, f) {
  print(paste("Retrieving ", f))
  # get rows from db
  rows_by_state_county_fips <- get_rows(cols_usda_curr_tax, f)
  # check completeness
  print(paste("Assessing completeness of ", f))
  completeness <- check_complete(rows_by_state_county_fips)
  # add fips column
  completeness[, fips_code := f]
  setcolorder(completeness, c("fips_code", "column", "blanks", "true_na", "written_na", "percent_complete", "unique_values"))
  completeness
}

