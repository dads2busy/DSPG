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
