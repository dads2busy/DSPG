library(here)
library(curl)
library(gtools)

# symlink - Sarah did this once. You should not run this
#system(command = "ln -s /project/biocomplexity/sdad/projects_data/ncses/stw/original/wioa_2021/ /sfs/qumulo/qhome/sm9dv/dspg21stw/data/dspg21stw/original/")



# how I download the files

#dest_file <- "/sfs/qumulo/qhome/sm9dv/dspg21stw/data/dspg21stw/originalwioa_2021/PY2019Q4.gz"
#url <- "https://www.dol.gov/sites/dolgov/files/eta/performance/pdfs/PY2019/WIOAPerformanceRecords_PY2019Q4_Public_csv.gz"
#download.file(url, dest_file, method = "libcurl")
#system(command = paste("gunzip -c ~/dspg21stw/data/dspg21stw/original/wioa_2021/PY2019Q4.gz > ~/dspg21stw/data/dspg21stw/original/wioa_2021/PY2019Q4.csv", sep = ""))



library(data.table)

### reading in 2019 Q4. use data.table because it is faster than read.csv() and this is a big file
data <- fread("~/dspg21stw/data/dspg21stw/original/wioa_2021/PY2019Q4.csv")
head(data)

names <- read.delim("~/dspg21stw/data/dspg21stw/original/wioa_2021/Q42019names.txt", header = F)

colnames(data) <- c(names$V1)
head(data)

### Get column names
library(tabulizer)
library(dplyr)
library(stringr)

# data dictionary pdf
out <- extract_tables("https://www.dol.gov/sites/dolgov/files/eta/performance/pdfs/PY2019/WIOA%20Performance%20Records%20Public%20Use%20File%20Record%20Layout%20PY2019Q4.pdf")

l <- list()
for (i in 1:length(out)){
  l[[i]]<- out[[i]][,1]
  l[[i]] <- as.character( na.omit(str_extract(l[[i]], "^\\d.+")))
}

list_of_names <- unlist(l)
list_of_names <- str_remove(list_of_names, "\\s.+$")


list_of_names <- str_replace(list_of_names, "\\W.+0", "-")
list_of_names

length(list_of_names)

# the PIRL titles 904.2 is not included
final_names <- list_of_names[!(list_of_names == "904.2")]

length(final_names)

final_names <- paste("PIRL", final_names, sep = " ")

options(useFancyQuotes = FALSE)
final_names <- dQuote(final_names)
#fwrite(list(final_names), file = "~/dspg21stw/data/dspg21stw/original/wioa_2021/Q42019names.txt", quote = F)


