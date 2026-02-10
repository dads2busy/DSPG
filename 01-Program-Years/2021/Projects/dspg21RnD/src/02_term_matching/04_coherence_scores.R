library(readr)
library(dplyr)
library(tidyr)

#nmf_oecd <- read_csv("~/git/dspg21RnD/data/dspg21RnD/nmf_OECD_10.csv")
nmf_oecd <- nmf_oecd%>%
  gather("iteration", "coherence_score", -n_topics)
nmf_oecd$n_topics <- as.factor(nmf_oecd$n_topics)

ggplot(data = nmf_oecd, aes(x = n_topics, y = coherence_score)) +
  geom_boxplot()  +
  labs(title = "NMF Coherence Score", subtitle = "AI Abstracts Using OECD Filtering Method", x= "Number of Topics", y = "Coherence Score (10 runs)") +
  theme_minimal()