
rm(list=ls())
library("stringr")
library("tidyr")
library("dplyr")
library("readr")
library("tidyverse")
library("RPostgreSQL")
source("~/git/dspg21oss/scripts/detect_sw.R")

setwd("/project/class/bii_sdad_dspg/uva_2021/dspg21oss/")
desciptions_classified <- read_rds("descriptions_classified.rds")

desciptions_classified_select <- desciptions_classified %>% 
  # parse the variables down to what we have finished 
  select(slug, description, language, #topics, commits, forks, stars, watchers, 
         prog_python, prog_rlang, prog_java, prog_javascript, prog_php, prog_clang, prog_stat_all,
         sys_windows, sys_linux, sys_mac, sys_android, sys_virtual, 
         app_blockchain_all, app_business_all, app_database_all, topics_ai, topics_dataviz)

colSums(desciptions_classified_select %>% select(-slug, -description,  -language))

# summary table 
summary_table <- data.frame(colSums(desciptions_classified_select %>% 
                                      select(-slug, -description,  -language)))
colnames(summary_table) <- "n"
summary_table <- summary_table %>% 
  rownames_to_column(var = "category") %>% 
  filter(!category %in% c("commits", "forks", "stars", "watchers", "prog_stat_all")) %>% 
  mutate(category_type = ifelse(test = str_detect(string = category, pattern = "^sys_"), yes = "system", no = ""),
         category_type = ifelse(test = str_detect(
           string = category, pattern = "^prog_"), yes = "programming", no = category_type),
         category_type = ifelse(test = str_detect(
           string = category, pattern = "^app_"), yes = "applications", no = category_type),
         category_type = ifelse(test = str_detect(
           string = category, pattern = "^topics_"), yes = "applications", no = category_type),
         category_type = ifelse(test = str_detect(
           string = category, pattern = "database"), yes = "system", no = category_type),
         category_type = ifelse(test = str_detect(
           string = category, pattern = "virtual"), yes = "system", no = category_type),
         category = str_replace(category, "sys_", ""),
         category = str_replace(category, "prog_", ""),
         category = str_replace(category, "app_", ""),
         category = str_replace(category, "topics_", ""),
         category = str_replace(category, "_all", ""),
         category = str_replace(category, "clang", "c"),
         category = str_replace(category, "rlang", "r"),
         category = str_replace(category, "dataviz", "visualization"),
         category = str_replace(category, "virtual", "virtualization"),
         category = str_replace(category, "\\b(ai)\\b", "ai/ml")) %>% 
  arrange(-n)
summary_table %>% 
  mutate(prc = round(n / 157538, 2))

setwd("/project/class/bii_sdad_dspg/uva_2021/dspg21oss/")
saveRDS(summary_table, "descriptions_summary.rds")

ggplot(data=summary_table, aes(x=reorder(category, n), y=n, fill=category_type)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(values=c("#56B4E9", "#232D4B", "#E57200")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("OSS Software Types Detected") +
  labs(caption = "Based on 7.3M GitHub Repo Descriptions") +
  scale_y_continuous(breaks=c(0, 200000, 400000, 600000, 800000), 
                     labels=c("0", "200k", "400k", "600k", "800k"))

