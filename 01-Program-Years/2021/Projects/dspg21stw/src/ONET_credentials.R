# Emily Kurtz
# ONET web scraping
# 6/11/21

SOCs<-read.csv("STW_2021.csv",header=TRUE)

library(rvest)


# without the .00 at the end
# i noticed if you go through the main page of the SOC code to get to the credentials, it uses the search term with the .00,
# so it is probably not best to exclude the .00
tbl<-data.frame(Certification.Name=character(),Certifying.Organization=character(),Type=character(),SOC=character())
for (oc in SOCs$SOC) {
  if(grepl(".",oc,fixed=TRUE)==FALSE){
    url <- paste0("https://www.careeronestop.org/Credentials/Toolkit/find-certifications.aspx?keyword=",
                  oc, "&direct=0&ajax=0&lang=en&pagesize=500")
    page <- read_html(url)
    tbls <- html_table(page, fill=TRUE)
    tbls<-data.frame(tbls)
    tbls$SOC<-rep(oc,nrow(tbls))
    tbl<-rbind(tbl,tbls)
  }
}
head(tbl)
dim(tbl)
write.csv(tbl,file="SOC_credentials.csv")






# 8 digit SOC ONETs
tbl<-data.frame(Certification.Name=character(),Certifying.Organization=character(),Type=character(),SOC=character())
for (oc in SOCs$SOC) {
  if(grepl(".",oc,fixed=TRUE)){
    url <- paste0("https://www.careeronestop.org/Credentials/Toolkit/find-certifications.aspx?keyword=",
                  oc, "&direct=0&ajax=0&lang=en&pagesize=500")
  }else{
    url <- paste0("https://www.careeronestop.org/Credentials/Toolkit/find-certifications.aspx?keyword=",
                  oc, ".00&direct=0&ajax=0&lang=en&pagesize=500")
  }
    page <- read_html(url)
    tbls <- html_table(page, fill=TRUE)
    tbls<-data.frame(tbls)
    tbls$SOC<-rep(oc,nrow(tbls))
    tbl<-rbind(tbl,tbls)
}
head(tbl)
dim(tbl)
write.csv(tbl,file="ONET_SOC_credentials.csv")










#Old question below. We decided to go ahead with doing both and comparing.

#I have a question. Do we want the .00 at the end of the SOC code or no? If so, we can just add .00 right before the
#&direct... in the second string of the url. One thing we want to note if so do that is that there are just a couple
#of SOCs in the data that already have a .0X. With those codes, we would not want to also add .00 at the beginning of
#the second string. This changes results. If we do not want to add .00 anyways, no need to change and all is good as is.





#what is the difference between .00s and non .00s?

test00<-data.frame(Certification.Name=character(),Certifying.Organization=character(),Type=character(),SOC=character())
for (oc in c("47-4011","47-4011.00","47-4011.01")) {
  url <- paste0("https://www.careeronestop.org/Credentials/Toolkit/find-certifications.aspx?keyword=",
                oc, "&direct=0&ajax=0&lang=en&pagesize=500")
  page <- read_html(url)
  test00s <- html_table(page, fill=TRUE)
  test00s<-data.frame(test00s)
  test00s$SOC<-rep(oc,nrow(test00s))
  test00<-rbind(test00,test00s)
}
test00
test00a<-test00[1:128,1]
test00b<-test00[129:nrow(test00),1]
test00a
test00b
all(test00b %in% test00a)
#it looks likely that the .0xs are a subset of the non-0xs.
# why are there leftover credentials though?
setdiff(test00a,test00b)
# something like Construction Documents Technologist shows up in four different occupations, none .0xs
tbl[tbl$Certification.Name=="Construction Documents Technologist",]








data<-read.csv("ONET_SOC_credentials.csv")
length(unique(data$Certification.Name))

