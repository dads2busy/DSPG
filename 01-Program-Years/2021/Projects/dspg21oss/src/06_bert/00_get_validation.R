library(readxl)
library(readr)
library(dplyr)
library("xlsx")


#ai
oss_software_labelled_ai_co <- read_excel("labelled_repo/oss_software_labelled_ai_co.xlsx")
oss_software_labelled_ai_co <- oss_software_labelled_ai_co%>%
  select(slug, ai_label, topics_ai)

#blockchain
oss_software_labelled_blockchain_co <- read_excel("labelled_repo/oss_software_labelled_blockchain_co.xlsx")
oss_software_labelled_blockchain_co <- oss_software_labelled_blockchain_co%>%
  select(slug, blockchain_label, app_blockchain_all)

#visualization
oss_software_labelled_dataviz_co <- read_excel("labelled_repo/oss_software_labelled_dataviz_co.xlsx")
oss_software_labelled_dataviz_co <- oss_software_labelled_dataviz_co%>%
  select(slug, dataviz_label, topics_dataviz)

clean_eng_github_repos_157k <- read_csv("clean_eng_github_repos_157k.csv")
colnames(clean_eng_github_repos_157k) = c("slug", "description_cleaneng")


clean_github_repos_157k <- read_csv("clean_github_repos_157k.csv")
colnames(clean_github_repos_157k) = c("slug", "description_clean1")

github_repos_157k <- read_csv("github_repos_157k.csv")

final_repo <- github_repos_157k%>%
  left_join(clean_github_repos_157k, by ="slug")%>%
  left_join(clean_eng_github_repos_157k, by ="slug")


#write ai
oss_software_labelled_ai_cz <- oss_software_labelled_ai_co%>%
  left_join(final_repo, by = "slug")%>%
  filter(!is.na(description))

#write.csv(oss_software_labelled_ai_cz, "/home/zz3hs/git/dspg21oss/data/dspg21oss/oss_software_labelled_ai_cz.csv")
#write.xlsx(oss_software_labelled_ai_cz, "oss_software_labelled_ai_cz.xlsx",sheetName = "ai", append = FALSE)

#write blockchain
oss_software_labelled_blockchain_cz <- oss_software_labelled_blockchain_co%>%
  left_join(final_repo, by = "slug")%>%
  filter(!is.na(description))

#write.csv(oss_software_labelled_blockchain_cz, "/home/zz3hs/git/dspg21oss/data/dspg21oss/oss_software_labelled_blockchain_cz.csv")
#write.xlsx(oss_software_labelled_blockchain_cz, "oss_software_labelled_blockchain_cz.xlsx")


#write dataviz
oss_software_labelled_dataviz_cz <- oss_software_labelled_dataviz_co%>%
  left_join(final_repo, by = "slug")%>%
  filter(!is.na(description))

#write.csv(oss_software_labelled_ai_cz, "/home/zz3hs/git/dspg21oss/data/dspg21oss/oss_software_labelled_ai_cz.csv")
#write.xlsx(oss_software_labelled_dataviz_cz, "oss_software_labelled_dataviz_cz.xlsx",sheetName = "dataviz", append = FALSE)

