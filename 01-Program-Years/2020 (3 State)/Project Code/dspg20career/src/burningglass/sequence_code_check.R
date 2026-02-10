for (pkg in c("tidyverse",  "data.table", "R.utils", "maditr", "stringr", "stringi", "dplyr", "ggplot2", "lubridate", "gt", "DataExplorer", "TraMineR", "TraMineRextras", "cluster", "tools")) {
  library(pkg, character.only = TRUE)
}
uva_color_palette <- 
  c("#232D4B", #space cadet
    "#2C4F6B", #indigo dye
    "#0E879C", #blue munsell
    "#60999A", #cadet blue
    "#D1E0BF", #tea green
    "#D9E12B", #pear
    "#E6CE3A", #citrine
    "#E6A01D", #marigold
    "#E57200" #princeton orange
  )

#function to clean the job data to identify post military jobs
clean_post_military <- function(bg_vet_job){
  bg_vet_job <- bg_vet_job%>%
    mutate(year_enter_job_market = year(date_enter_job_market))%>%
    dplyr::select(-noofjobs, -sector, -tenure)
  
  #veterans and the date they ended their last ONET 55 job
  vet_endmilitary <- bg_vet_job%>%
    mutate(date_end_onet55 = if_else(is_onet55==T, enddate, as.Date(NA)))%>%
    filter(!is.na(date_end_onet55))%>%  #exluce people who don't have valid onet55 code
    dplyr::select(id, date_end_onet55) %>%
    #keep the latest onet55 job
    group_by(id)%>%   
    arrange(desc(date_end_onet55))%>%
    group_by(id)%>%
    distinct(id, .keep_all = TRUE) 
  
  bg_vet_job  <- inner_join(bg_vet_job, vet_endmilitary, by = "id")
  
  ## sequence data
  post_military_seq <- bg_vet_job %>%
    dplyr::select(id, end_year, onet_job_zone, startdate, enddate, job_duration_day, date_end_onet55, year_enter_job_market)%>%
    mutate(year_end_onet55 = year(date_end_onet55))%>%
    dplyr::select(-year_enter_job_market)%>%
    filter(startdate >= date_end_onet55)%>% #find jobs that came after the date ended onet55 job
    filter(onet_job_zone != 55)  %>%  #filter out 55 jobs that have the same start and end date  
    mutate(start_year = year(startdate))%>%
    #transform from calender year to year start sequence analysis
    mutate(start_year = start_year - year_end_onet55 + 1)%>%  
    mutate(end_year = end_year - year_end_onet55 + 1) %>%
    dplyr::select(id, start_year, end_year, onet_job_zone)%>%
    #if two jobs have the same start and end year, we chose the one with higher job zone
    group_by(id)%>%
    arrange(desc(onet_job_zone))%>%
    group_by(id)%>%
    distinct(id, start_year, end_year, .keep_all = TRUE)
  
  return(post_military_seq)
}

#read in vet job data
bg_vet_job <-read_csv("~/git/dspg20career/data/04_bg_vet_job.csv")

#perform the cleaning function
bg_vet_job_seq <- clean_post_military(bg_vet_job)

bg_vet_job_seq <- as.matrix(bg_vet_job_seq)
bg_vet_job_seq <- as.data.frame(bg_vet_job_seq)

# Convert sequence data from SPELL format to STS format
sts_vet <- seqformat(bg_vet_job_seq, from = "SPELL", to = "STS",
                     id = "id",  begin = "start_year", end = "end_year", 
                     status = "onet_job_zone", process = FALSE)

#obtain jobs that appear after 10 years they exit military 
sts_vet <- sts_vet[, 1:10]

# Here we are renaming columns to be in format "yn" (year in the job market)
names(sts_vet) <- paste0("y", 1:ncol(sts_vet))

#sequence object
vet.seq <- seqdef(sts_vet, 
                  left="Military Transitional Unemployment", gaps="Civilian Unemployment", right="Retirement", 
                  cpal = c("#CBBEB5",  uva_color_palette[1], uva_color_palette[2],uva_color_palette[4],  uva_color_palette[6], uva_color_palette[9], "#CBBEB5", "#CBBEB5","#CBBEB5")
)


# Compute transition matrix (with standardization)
transition_matrix_count <- seqtrate(vet.seq, weighted=FALSE, count=TRUE) #using count
diag(transition_matrix_count) = 0  #make the diagnal 0

#calculate rowsum
rowsum <- apply(transition_matrix_count, 1, sum)
n_stage <- dim(transition_matrix_count)[1]
#initialize a transition matrix
transition_matrix_standardize <- matrix(NA, n_stage, n_stage)

#calculate percent 
for(i in 1:n_stage) {
  for (j in 1:n_stage){
    transition_matrix_standardize[i,j] = transition_matrix_count[i,j]/rowsum[i]
  }
}

#code NAs as 0
for(j in 1:ncol(transition_matrix_standardize)){
  transition_matrix_standardize[9,j] = transition_matrix_standardize[9,j] = 0
}
#round(transition_matrix_standardize,2)

# Convert transition matrix to cost matrix
cost_matrix_standardized <- matrix(NA, n_stage, n_stage) #initiate an empty cost matrix
for (i in 1:n_stage){
  for(j in 1:n_stage){
    cost_matrix_standardized[i,j] = 2-transition_matrix_standardize[i,j]-transition_matrix_standardize[j,i]
  }
}

#make the diagnal 0
diag(cost_matrix_standardized) = 0

# clustering
# vet.seq.OM <- seqdist(vet.seq, method = "OM", indel = 3, sm = cost_matrix_standardized, with.missing = TRUE)
# clusterward <- agnes(vet.seq.OM, diss = TRUE, method = "ward")
# saveRDS(clusterward, file = "data/clusterward_onet55_10yrs_standardized_matrix.rds")

clusterward <- readRDS(file = "~/git/dspg20career/data/clusterward_onet55_10yrs_normalized_matrix.rds")

#dendrogram 
# plot(clusterward, which.plots =2)
# abline(h=200, col="purple")
# abline(h=90, col="red")
cluster8 <- cutree(clusterward, k=8)
cluster8 <- factor(cluster8, labels = c("Type 1",  "Type 2", "Type 3", "Type 4", "Type 5", "Type 6", "Type 7", "Type 8"))
table(cluster8)

#longitudinal plot
seqfplot(vet.seq, group = cluster8, pbarw = T)

#another cluster plot
seqmtplot(vet.seq, group = cluster8)





