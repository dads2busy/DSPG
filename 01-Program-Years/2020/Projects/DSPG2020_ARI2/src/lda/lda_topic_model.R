library(dplyr)
library(tidyverse)
library(tidytext)
library(stm)
library(quanteda)
library(ggplot2)
library(readr)

job_perf <- read_csv("/project/class/bii_sdad_dspg/uva/dod_ari2/ari2_07_21.csv")

#Grouping by scale source/citation if we want to consider texts as documents instead of individual scale items
cols <- c('STEM', 'ITEM')
job_perf$scale <- apply( job_perf[ , cols ], 1 , paste, collapse = " ")

job_perf <- job_perf %>%
  group_by(CITATION, SCALE_NAME, SOURCE_DOMAIN, YEAR) %>%
  summarise(scale = paste0(scale, collapse = " "))

#STM Model
processed.scale <- textProcessor(documents = job_perf$scale, metadata = job_perf, lowercase = TRUE,
                                 removestopwords = FALSE, removenumbers = TRUE,
                                 removepunctuation = TRUE, ucp = FALSE, stem = FALSE,
                                 wordLengths = c(0, Inf), sparselevel = 1, language = "en",
                                 verbose = TRUE, onlycharacter = FALSE, striphtml = FALSE,
                                 customstopwords = "na", custompunctuation = NULL, v1 = FALSE)

out.scale <- prepDocuments(processed.scale$documents, vocab = processed.scale$vocab,
                           meta = processed.scale$meta, lower.thresh = 0, upper.thresh = Inf)

docs.scale <- out.scale$documents
vocab.scale <- out.scale$vocab
meta.scale <- out.scale$meta

perfSelect <- selectModel(out.scale$documents, out.scale$vocab, K = 5,
                          max.em.its = 200, data = out.scale$meta, runs = 20, init.type = "LDA",
                          seed = 2020)
plotModels(perfSelect)
selectedmodel <- perfSelect$runout[[4]]

kresult <- searchK(out.scale$documents, out.scale$vocab, init.type = "LDA",
                   K = c(5, 8, 10), seed = 2020, data = out.scale$meta)
plot(kresult)

perfPrevFit2 <- stm(out.scale$documents, vocab = out.scale$vocab,
                   K = 5, data = out.scale$meta, init.type = "LDA", max.em.its = 100)

topicQuality(perfPrevFit, out.scale$documents)

labelTopics(perfPrevFit)

mod.out.corr <- topicCorr(perfPrevFit)
plot(mod.out.corr)

plot(perfPrevFit, type = "summary", xlim = c(0, 1), labeltype = "frex")

wordclouds <- function(topic) {
  cloud(perfPrevFit, topic = topic)
}

topicnum <- 1:5
for (i in topicnum) {
  wordclouds(i)
}
