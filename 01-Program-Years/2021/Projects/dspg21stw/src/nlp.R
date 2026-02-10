# NLP

onet<-read.csv("ONET_SOC_credentials.csv",header=T)
bgt<-read.csv("bgt_credentials.csv",header=T)
va<-read.csv("VACommunityColleges.csv",header=TRUE)

head(onet)
head(bgt)




excludeNameMatch<-bgt$Var1 %in% onet$Certification.Name == TRUE
excludeAgencyMatch<-
  
  
  

  
  
  
  
  
  
  
#haleigh things
certlist<-unique(onet$Certification.Name)
vectorlist<-list()
for(i in 1:length(certlist)){
  vectorlist[[i]]<-c(onet[onet$Certification.Name==certlist[i],4])
}

adjacencyMatrix<-matrix(rep(0,length(certlist)^2),nrow=length(certlist))
for(i in 1:length(certlist)){
  for(j in 1:length(certlist)){
    if(sum(vectorlist[[i]] %in% vectorlist[[j]])>0){
      adjacencyMatrix[i,j]<-1
    } 
  }
}








# Libraries 
library(stringr)
library(readxl)
library(fuzzywuzzyR)
library(gsubfn)

# Reading in credential data 
# write.table(name_count, file = "bgt_credentials.txt", sep = ",")
bgt_credentials <- read.csv("bgt_credentials.csv")
colnames(bgt_credentials) <- c('X','Certification.Name','Count')
bgt_credentials <- as.data.frame(bgt_credentials)

ONET_credentials <- read_excel("ONET_SOC_credentials.xlsx")
ONET_credentials <-unique(ONET_credentials$Certification.Name)

View(bgt_credentials)
View(ONET_credentials)
table(bgt_credentials$Var1 %in% ONET_credentials$Certification.Name)

# Remove spaces and periods 

bgt_credentials <- gsub(" ", "_", bgt_credentials$Certification.Name)


bgt_credentials <- gsub(".", "_", bgt_credentials)

ONET_credentials <- gsub(" ", "_", ONET_credentials$Certification.Name, fixed = TRUE)
ONET_credentials <- gsub(".", "_", ONET_credentials$Certification.Name, fixed = TRUE)











# picking stop words by seeing what top words are
library(tidytext)
library(dplyr)
library(widyr)
onet<-read.csv("ONET_SOC_credentials.csv",header=T)
bgt<-read.csv("bgt_credentials.csv",header=T)
va<-read.csv("VACommunityColleges.csv",header=TRUE)
onet_words <- onet %>% unnest_tokens(word,Certification.Name) %>% count(word,sort=T)
head(onet_words,25)
bgt_words <- bgt %>% unnest_tokens(word,Var1) %>% count(word,sort=T)
head(bgt_words,25)
va_words <- va %>% unnest_tokens(word,Credential) %>% count(word,sort=T)
head(va_words,25)


# remove spaces, stop words, and lowercase things
bgt$Var1 <- gsub(" ", "", bgt$Var1)
onet$Certification.Name<-tolower(onet$Certification.Name)
removeList<-c("certified ","certification ","and ","on ","in "," ")
for(remove in removeList){
  onet$Certification.Name<-gsub(remove,"",onet$Certification.Name)
}
head(onet)
test<-adist(onet$Certification.Name,va$Credential)
pairwise_similarity()










