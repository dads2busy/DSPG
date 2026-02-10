# load packages
library(tidyverse)
library(tidytext)
library(wordcloud)
library(stringr)


# read in File
descrips <- read_csv("~/git/dspg21oss/data/dspg21oss/github_repos_157k.csv")

clean_descrips <- descrips

# remove links starting with "http"
clean_descrips$description <- str_remove_all(clean_descrips$description, "http\\S+\\s*")
# test it out
# a <- "https://www.stackoverflow.com is a link"
# a <-str_remove_all(a, "http\\S+\\s*")
# print(a)

# remove symbols
clean_descrips$description <- str_replace_all(clean_descrips$description,"[^[:alnum:]]", " ")

# remove numbers
# HAVING TROUBLE WITH THIS???
clean_descrips$description <- str_replace_all(clean_descrips$description,"^[0-9]+", " ")

# remove non-english characters
clean_descrips$description <- str_remove_all(clean_descrips$description, "[^[\\da-zA-Z ]]")

# try to identify lang
clean_descrips$cld3<-lapply(lang$description, cld3::detect_language)
clean_descrips$cld2<-lapply(lang$description, cld2::detect_language)

clean_descrips <- lang %>% 
  filter(cld3=="en"|cld2=="en")

# select slug and description
small_clean_descrips <- clean_descrips %>% 
  select(slug, description)

# save file
write_csv(small_clean_descrips,"/home/dab3dj/git/dspg21oss/data/dspg21oss/clean_eng_github_repos_157k.csv")
