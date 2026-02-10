# MATCH STRINGS, produces table of matches

library(stringdist)

match_strings <- function(column, table, threshold){

# Computing string distance matrix for all assignments
matrix <- stringsimmatrix(column, column)
# Keep only the upper triangle
matrix[lower.tri(matrix, diag = FALSE)] <- 0
# Filter for non-exact matches and anything about 0.7
matches <- as.data.frame(which(matrix >= threshold & matrix != 1, arr.ind = TRUE)) %>% filter(row != col)

data <- table %>% tibble::rownames_to_column() %>% mutate(rowname = as.integer(rowname)) %>% left_join(matches, by = c("rowname" = "row"))
data <- data %>% mutate(match = ifelse(!is.na(col), col, rowname)) %>% 
  mutate(Assignment = Assignment[match]) %>% select(-col, -rowname, -match) %>% distinct()

}