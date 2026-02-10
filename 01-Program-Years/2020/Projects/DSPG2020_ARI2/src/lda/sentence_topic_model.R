library(dplyr)
library(tidyverse)
library(tidytext)
library(stm)
library(quanteda)
library(ggplot2)
library(readtext) ##cannot get readtext package to install because library(pdftools) won't install

job_perf <- read.csv("/project/class/bii_sdad_dspg/uva/dod_ari2")

#Grouping by scale source/citation if we want to consider texts as documents instead of individual scale items
job_perf <- job_perf %>%
  group_by(CITATION) %>%
  summarise(scale = paste0(ITEM, collapse = " "))

#Using quanteda to look at different aspects of our corpus
scale_corpus <- corpus(job_perf)
summary(scale_corpus)

texts(scale_corpus)[2]

tokenInfo <- summary(scale_corpus)

ggplot(data = tokenInfo, aes(x = Year, y = Tokens, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(labels = c(seq(1970, 2020, 12)), breaks = seq(1970, 2020, 12)) +
  theme_bw()

tokenInfo[which.max(tokenInfo$Tokens), ]

kwic(scale_corpus, "performance")

tokens(scale_corpus, remove_numbers = TRUE, remove_punct = TRUE)
#Tokenize by sentence
tokens(scale_corpus, remove_numbers = TRUE, remove_punct = TRUE, what = "sentence")

?dfm
