# CLEANS ASSIGNMENTS FOR ALL FILES IN THE DATA FOLDER, COMBINES THEM INTO ONE TABLE

clean_assignments <- function(path_to_data = "./data"){
  
  files <- list.files(path_to_data) %>% str_subset(pattern = "\\.csv$")
  tables <- files[!str_detect(files, "GPT|slashes|GT")]
  
  lookup <- c("Type" = "Assignment.Type", "Assignment" = "Assignments")
  
  table <- NULL
  big_table <- NULL
  
for(i in 1:length(tables)){
  table <- read.csv(paste0(path_to_data, "/", tables[i]))
  table <- table %>% rename(any_of(lookup)) %>% select(any_of(c("MOS", "Assignment", "Type", "Rank")))
  big_table <- rbind(big_table, table)
}

slashes <- read.csv(paste0(path_to_data, "/slashes.csv"))

library(mgsub)

big_table <- big_table %>% mutate(Assignment = tolower(Assignment)) %>%
  mutate(New_Assignment = mgsub(Assignment, slashes$x, slashes$reconciled)) %>%
  separate_rows(New_Assignment, sep = c(",")) %>% separate_rows(New_Assignment, sep = " or ") %>% separate_rows(New_Assignment, sep = " and ") %>%
  mutate(New_Assignment = trimws(New_Assignment)) %>% mutate(New_Assignment = case_when(
    str_detect(Assignment, "executive officer") ~ str_replace(New_Assignment, "executive officer", "xo"),
    str_detect(Assignment, "regiment|garrison") ~ str_replace(New_Assignment, "regiment|garrison", "battalion"),
    str_detect(Assignment, "s-3") ~ str_replace(New_Assignment, "s-3", "s3"),
    str_detect(Assignment, "commander") & !(str_detect(New_Assignment, "commander")) ~ paste0(New_Assignment, " commander"),
    str_detect(Assignment, "staff officer") & !(str_detect(New_Assignment, "staff officer")) ~ paste0(New_Assignment, " staff officer"),
    str_detect(Assignment, "staff head") & !(str_detect(New_Assignment, "staff head")) ~ paste0(New_Assignment, " staff head"),
    str_detect(Assignment, "staff") & !(str_detect(New_Assignment, "staff")) ~ paste0(New_Assignment, " staff"),
    str_detect(Assignment, "developer") & !(str_detect(New_Assignment, "developer")) ~ paste0(New_Assignment, " developer"),
    str_detect(Assignment, "instructor") & !(str_detect(New_Assignment, "instructor")) ~ paste0(New_Assignment, " instructor"),
    str_detect(Assignment, "rotc") & !(str_detect(New_Assignment, "rotc")) ~ paste0("rotc ", New_Assignment),
    str_detect(Assignment, "head") & !(str_detect(New_Assignment, "head")) ~ paste0(New_Assignment, " head"),
    str_detect(Assignment, "battalion") & !(str_detect(New_Assignment, "battalion|brigade")) ~ paste0("battalion ", New_Assignment),
    str_detect(Assignment, "brigade") & !(str_detect(New_Assignment, "brigade|battalion|division|core|joint|regiment|corps")) ~ paste0("brigade ", New_Assignment),
    str_detect(Assignment, "u.s. army recruiting company ") & !(str_detect(New_Assignment, "u.s. army recruiting company ")) ~ paste0("u.s. army recruiting company ", New_Assignment),
    str_detect(Assignment, "usma") & !(str_detect(New_Assignment, "usma")) ~ paste0("usma ", New_Assignment),
    str_detect(Assignment, "generalist") & !(str_detect(New_Assignment, "generalist")) ~ paste0(New_Assignment, " generalist"),
    str_detect(Assignment, "s3") & !(str_detect(New_Assignment, "s3|xo")) ~ paste0(New_Assignment, " s3"),
    str_detect(Assignment, "xo") & !(str_detect(New_Assignment, "s3|xo")) ~ paste0(New_Assignment, " xo"),
    str_detect(Assignment, "duty") & !(str_detect(New_Assignment, "duty")) ~ paste0(New_Assignment, " duty"),
    str_detect(Assignment, "assistant") & !(str_detect(New_Assignment, "assistant")) ~ paste0(New_Assignment, " assistant"),
    str_detect(Assignment, "assignments") & !(str_detect(New_Assignment, "assignments")) ~ paste0(New_Assignment, " assignments"),
    str_detect(Assignment, "army-sponsored") & !(str_detect(New_Assignment, "army-sponsored")) ~ paste0("army-sponsored", New_Assignment),
    .default = New_Assignment
  )) %>% mutate(New_Assignment = trimws(New_Assignment)) %>% mutate(New_Assignment = sub('[[:punct:]]+$|s+$', '', New_Assignment)) %>%
  mutate(Rank = case_when(
    Rank == "Lieutenant" ~ "LT",
    Rank %in% "Captain|CAP" ~ "CPT",
    Rank == "Major" ~ "MAJ",
    Rank == "Lieutenant Colonel" ~ "LTC",
    Rank == "Colonel" ~ "COL",
    .default = Rank
  )) %>% select(-Assignment, Assignment = New_Assignment)

}
