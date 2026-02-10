library(tidyverse)
library(readxl)
library(tidytext)
library(BTM)
library(textplot)
library(ggraph)
library(concaveman)


job_perf <- read_excel("/project/class/bii_sdad_dspg/uva/dod_ari2/DSPG-ARI2 -- Performance Item and Scale Database.xlsx",
                       sheet = "Performance Items")

job_perf <- job_perf %>%
  filter(CITATION != "Borman, W. C., Motowidlo, S. J., Rose, S. R., & Hanser, L. M. (1987). Development of a Model of Soldier Effectiveness: Retranslation Materials and Results. HUMAN RESOURCES RESEARCH ORGANIZATION ALEXANDRIA VA.")

cols <- c('STEM', 'ITEM')
job_perf$scale <- apply( job_perf[ , cols ], 1 , paste, collapse = " ")
job_perf$scale <- gsub("NA",' ', job_perf$scale)

job_perf$id <- c(1:2249)

job_bi <- tibble(id = job_perf$id, text = job_perf$scale)

View(job_bi)

tidy_bi <- job_bi %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

perf_model  <- BTM(tidy_bi, k = 12, beta = 0.01, iter = 1000, window = 7, trace = 100)

perf_model$theta

topicterms <- terms(perf_model, top_n = 10)
topicterms

plot(perf_model, top_n = 4)

library(LDAvis)
docsize <- table(tidy_bi$id)
scores <- predict(perf_model, tidy_bi)
scores <- scores[names(docsize), ]

json <- createJSON(
  phi = t(perf_model$phi),
  theta = scores,
  doc.length = as.integer(docsize),
  vocab = perf_model$vocabulary$token,
  term.frequency = perf_model$vocabulary$freq)

json <- createJSON(
  phi = t(perf_model$phi),
  theta = scores,
  doc.length = as.integer(docsize),
  vocab = perf_model$vocabulary$token, term.frequency = perf_model$vocabulary$freq)

serVis(json)
