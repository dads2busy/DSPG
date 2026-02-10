library(tidyverse)
library(leaflet)

ffu_va <- read_csv('~/git/dspg20broadbandERS/data/acs-cl-joined/ffu_va.csv')

# functions to calculate completeness (from Devika)
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
  percent_complete <- (len - (blanks + true_na + written_na)) / len
  unique_values <- length(unique(vec))
  tibble(blanks = blanks,
         true_na = true_na,
         written_na = written_na,
         percent_complete = percent_complete,
         unique_values = unique_values)
}
check_complete <- function(df) {
  z <- deparse(substitute(df))
  map_df(.x = df, .f = check_calc) %>%
    mutate(column = colnames(df)) %>%
    mutate(set = print(z))  %>%
    select(set, column, blanks, true_na, written_na, percent_complete, unique_values)
}

ffu_va_complete <- check_complete(ffu_va)

ffu_va_long <- ffu_va %>%
  gather(key = ffu, value = value, 2:40)

ffu_grouped_by_variable <- ffu_va_long %>%
  group_by(ffu) %>%
  summarize(mean_ffu = mean(value, na.rm = TRUE),
            n= n()) %>%
  mutate(bad = ifelse((mean_ffu > 1 | mean_ffu < -1), TRUE, FALSE),
         verybad = ifelse((mean_ffu > 1.5 | mean_ffu < -1.5), TRUE, FALSE))

length(which(ffu_grouped_by_variable$bad))
length(which(ffu_grouped_by_variable$verybad))

ffu_grouped_by_geoid <- ffu_va_long %>%
  group_by(GEOID) %>%
  summarize(n=n())
