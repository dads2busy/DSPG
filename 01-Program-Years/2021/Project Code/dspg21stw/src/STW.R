# STW ideas as of 2021-0709

data<-read.csv("ONET_SOC_credentials.csv",header=T)
head(data)
length(unique(data$SOC))
# 22 STW jobs call for no credentials at all
length(unique(data$Certification.Name))
nrow(data)
frequencies<-sort(table(data$Certification.Name),decreasing=T)
head(frequencies)
