library(tidyverse)
library(lubridate)
library(TraMineR)
library(TraMineRextras)
library(dplyr)


### Aligned sequences for veterans, we need one table of sequences and one of first job years -----
# Check file path, loading the clean dataset for sequence analysis
bg_vet_job <- read_csv("~/git/DSPG2020/career/data/04_bg_vet_job.csv")

# We are choosing years as our unit of analysis because months are unreliable. We are also taking the highest O*NET job zone for a given year to deal with overlapping jobs.
bg_vet_job_seq <- bg_vet_job %>%
  mutate(startyear = year(startdate), endyear = year(enddate)) %>%
  select("id", "startyear", "endyear", "onet_job_zone")%>%
  group_by(id)%>%
  arrange(desc(onet_job_zone))%>%
  group_by(id)%>%
  distinct(id, startyear, endyear, .keep_all = TRUE)

# We need an auxillary table with the year of entering the job market to align the sequences
bg_vet_job_first <- bg_vet_job_seq %>% 
  select("id", "startyear") %>% group_by(id) %>% transmute(enter = min(startyear)) %>% distinct() %>% ungroup()

# The seqformat() function does not like any data that has been previously grouped. Here we are "resetting" the data so that it will pass through the function.
bg_vet_job_seq <- as.matrix(bg_vet_job_seq)
bg_vet_job_seq <- as.data.frame(bg_vet_job_seq)
bg_vet_job_first <- as.matrix(bg_vet_job_first)
bg_vet_job_first <- as.data.frame(bg_vet_job_first)

# The input for the function is the prepared sequence table. The data is in format SPELL and we are transforming to format STS. By setting process = TRUE we can align the sequences using the prepared auxillary table.
sts_vet <- seqformat(bg_vet_job_seq, from = "SPELL", to = "STS",
                     id = "id",  begin = "startyear", end = "endyear", 
                     status = "onet_job_zone", process = TRUE,
                     pdata = bg_vet_job_first, pvar = c("id", "enter"))
# Here we are renaming columns to be in format "yn" (year in the job market)
names(sts_vet) <- paste0("y", 1:100)

# Writing the final table as a csv
write.csv(sts_vet, "sts_vet.csv")

# First 10 years

# The input for the function is the prepared sequence table. The data is in format SPELL and we are transforming to format STS. By setting process = TRUE we can align the sequences using the prepared auxillary table.
sts_vet <- seqformat(bg_vet_job_seq, from = "SPELL", to = "STS",
                     id = "id",  begin = "startyear", end = "endyear", 
                     status = "onet_job_zone",
                     process = TRUE, pdata = bg_vet_job_first, 
                     pvar = c("id", "enter"))
# Here we are renaming columns to be in format "yn" (year in the job market)
names(sts_vet) <- paste0("y", 1:100)

# Writing the final table as a csv
write.csv(sts_vet, "sts_vet.csv")

## Aligned sequences for all jobs (This will take a while to run, I haven't gotten it yet) ---------
set.seed(01301998)
bg_all_job %>% group_by(id) %>% sample_n(8067, replace = FALSE)

bg_all_job_seq <- bg_all_job %>%
  mutate(startyear = year(startdate), endyear = year(enddate)) %>%
  select("id", "startyear", "endyear", "onet_job_zone")%>%
  group_by(id)%>%
  arrange(desc(onet_job_zone))%>%
  group_by(id)%>%
  distinct(id, startyear, endyear, .keep_all = TRUE)

bg_all_job_first <- bg_all_job_seq %>% 
  select("id", "startyear") %>% group_by(id) %>% transmute(enter = min(startyear)) %>% distinct() %>% ungroup()

bg_all_job_seq <- as.matrix(bg_all_job_seq)
bg_all_job_seq <- as.data.frame(bg_all_job_seq)
bg_all_job_first <- as.matrix(bg_all_job_first)
bg_all_job_first <- as.data.frame(bg_all_job_first)

sts_all <- seqformat(bg_all_job_seq, from = "SPELL", to = "STS",
                     id = "id",  begin = "startyear", end = "endyear", 
                     status = "onet_job_zone", process = TRUE,
                     pdata = bg_all_job_first, pvar = c("id", "enter"))
names(sts_all) <- paste0("y", 1:100)

write.csv(sts_all, "sts_vet.csv")

seq <- seqdef(sts_vet[,-1])

seqmtplot(seq, ylim = c(0, 10), border = NA, with.legend= FALSE,cpal = c("#D1E0BF",  "#60999A", "#0E879C","#2C4F6B", "#232D4B"))

seqmsplot(seq, with.legend = "right",  cpal = c("#D1E0BF",  "#60999A", "#0E879C","#2C4F6B", "#232D4B"), border = NA)

seqdplot(seq, xlim = c(0, 46), border = NA, with.legend = "right", cpal = c("#D1E0BF",  "#60999A", "#0E879C","#2C4F6B", "#232D4B"))

, #space cadet
"#2C4F6B", #indigo dye
"#0E879C", #blue munsell
"#60999A", #cadet blue
"#D1E0BF"