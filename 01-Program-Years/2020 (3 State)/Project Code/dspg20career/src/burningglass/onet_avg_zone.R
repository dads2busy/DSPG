library(tidyverse)

# see which soc codes are missing from onet, mostly "all other" jobs and mililtary jobs
bg_soc_codes <- job %>% select(onet) %>% distinct()
onetsoc_code <- onet$onetsoc_code
bg_soc_codes %>% mutate(match = onet %in% onetsoc_code) %>% filter(match == FALSE)

## plan: add missing onet codes to onet, mutate a variable for group, calculate mean job zone by group
## here we are grouping by the first four (XX-XX) digits of the onet code, making the assumption that the all, other job is alike enough to be assigned the average job zone of the group
## we want to report the average zone only for soc codes that don't have a job zone

bg_soc_codes <- job %>% select(onet) %>% distinct()
onetsoc_code <- onet$onetsoc_code
missing_soc_codes <- bg_soc_codes %>% mutate(match = onet %in% onetsoc_code) %>% filter(match == FALSE) %>% rename(onetsoc_code = onet) %>% select(onetsoc_code) %>% mutate(title = NA, job_zone = NA, date_updated = NA, domain_source = NA)

onet <- rbind(onet, missing_soc_codes)

onet <- onet %>% mutate(group_soc = str_extract(onetsoc_code, "^[0-9][0-9]-[0-9][0-9]")) %>% group_by(group_soc) %>% mutate(avg_zone = round(mean(job_zone, na.rm = TRUE))) %>% mutate(new_zone = ifelse(is.na(job_zone), avg_zone, job_zone))

write.csv(onet, "~/git/DSPG2020/career/src/burningglass/onet_avg_zone.csv")
