
library(dplyr)
library(tidyr)

domains <- read.csv("country_domain_list.csv",strip.white=TRUE)
head(domains)

trim.trailing <- function (x) sub("\\s+$", "", x)

domains$Country <- gsub(" ","",domains$Country)
domains$domain <- trim.trailing(domains$domain)

#load the cran data that has email domains of authors
df7 <- read.csv("df7.csv",strip.white=TRUE)
head(df7)

#get the last part of email domains

df7$Maintainer.email.domain.end <- stringr::str_match(df7$Maintainer.email.domain, "\\.[^\\.]*$") 

dim(df7)
df8 <- merge(df7,domains, by.x="Maintainer.email.domain.end", by.y="domain",all.x=T, all.y=F)

#df8 <- df8[,c(2:11,1,39,12:38)]

df9 <- df8 %>%
  group_by(Maintainer.email.domain.end) %>%
  mutate(email.domain_end_count = n_distinct(name))

df10 <- df9 %>%
  group_by(Maintainer.email.domain.end) %>%
  mutate(email.domain_end_unique_name = n_distinct(Maintainer.name))

df10 <- df10[,c(1:11,40,41,12:39)]

length(unique(df10$Maintainer.name))

colnames(df10)

file2 <- unique(df10[,c(11:14)])

file2_countrydomain <- file2[!is.na(file2$Country),]

write.csv(file2_countrydomain,"file2-cran_domain_end_country.csv")


file3 <- file2[is.na(file2$Country),]

write.csv(file3,"file3-cran_domain_end_excl_country.csv")


ed <- df10[which(df10$Maintainer.email.domain.end == ".edu"),]

colnames(ed)

file4 <- unique(ed[,c(8:11),])

write.csv(file4,"file4-cran_domain_edu.csv")


gov <- df10[which(df10$Maintainer.email.domain.end == ".gov"),]

file5 <- unique(gov[,c(8:11),])

write.csv(file5,"file5-cran_domain_gov.csv")

