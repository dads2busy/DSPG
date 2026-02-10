### cleaning up the three text matching tables and making a nice user friendly website display


#read in the tables
onetbgt<-read.csv("ONETxBGT.csv",header=T)
onetva<-read.csv("ONETxVA.csv",header=T)
vabgt<-read.csv("VAxBGT.csv",header=T)

#purge the columns we don't want to display
head(onetbgt)
onetbgt<-onetbgt[,c(2,3,5,7,8)]
head(onetbgt)
colnames(onetbgt)<-c("ONET SOC","ONET Certification","Confidence of Match","Burning Glass SOC","Burning Glass Certification")
onetbgt<-onetbgt[,c(1,2,3,5,4)]
#purge the rows where ONET SOC == ONET Cert because those are "generics"; size was inititally 10776 rows
onetbgt<-onetbgt[onetbgt$'ONET SOC'!=onetbgt$'ONET Certification',]
# now has 8226 rows

#remove SOCS
onetbgt<-onetbgt[,c(2,3,4)]
onetbgt<-onetbgt[!duplicated(onetbgt[c(1,3)]),] #finally, 1381

head(onetbgt[order(onetbgt$`Confidence of Match`,decreasing=TRUE),])
onetxbgt<-onetbgt[order(onetbgt$`Confidence of Match`,decreasing=TRUE),]
onetxbgt<-onetxbgt[,c(1,3)]
write.csv(onetxbgt,"onetxbg.csv")
hist(onetbgt$'Confidence of Match')
nrow(onetbgt[onetbgt$'Confidence of Match'>0.4,])

onetbgt<-onetbgt[,c(2,4)]
write.csv(onetbgt,"onetxbg.csv")






#playing
onetbgt<-read.csv("ONETxBGT.csv",header=T)
confbysoc<-onetbgt[,c(5:7)]
confbysoc$SOCGroup<-substr(confbysoc$SOC,1,2)
par(mgp=c(3,1,0))
par(mar=c(5,30,4,2)+0.1)
boxplot(confidence_cert~SOCGroup,confbysoc,ylab="",xlab="Confidence of Match",
        main="Confidence of Credential Match (BGT to O*NET) by Occupational Group",col="#E87404",horizontal = T,
        names=c("Management","Business and Financial Operations",
                "Architecture and Engineering Occupation","Life, Physical, and Social Science",
                "Art, Design, Entertainment, Sports, And Media","Healthcare Practitioners and Technical Occupation",
                "Protective Service","Food Preparation and Serving Related",
                "Office and Administrative Support","Farming, Fishing, and Forestry",
                "Construction and Extraction","Installation, Maintenance, and Repair","Production",
                "Transportation and Material Moving"),las=1)
abline(v=median(confbysoc$confidence_cert),col="blue")


conf<-vabgt[,c(5,7)]
conf$SOCGroup<-substr(conf$SOC,1,2)
par(mgp=c(3,1,0))
par(mar=c(5,30,4,2)+0.1)
boxplot(confidence_cert~SOCGroup,conf,ylab="",xlab="Confidence of Match",
        main="Confidence of Credential Match (Burning Glass to Virginia Community Colleges) by Occupational Group",col="#E87404",horizontal = T,
        names=c("Management","Business and Financial Operations",
                "Architecture and Engineering Occupation","Life, Physical, and Social Science",
                "Art, Design, Entertainment, Sports, And Media","Healthcare Practitioners and Technical Occupation",
                "Protective Service","Food Preparation and Serving Related",
                "Office and Administrative Support","Farming, Fishing, and Forestry",
                "Construction and Extraction","Installation, Maintenance, and Repair","Production",
                "Transportation and Material Moving"),las=1)





conf<-onetva[,c(5,9)]
par(mgp=c(3,1,0))
par(mar=c(5,30,4,2)+0.1)
boxplot(confidence_cert~Awarding.Entity,conf,ylab="",xlab="Confidence of Match",
        main="Confidence of Credential Match (Burning Glass to Virginia Community Colleges) by Occupational Group",col="#E87404",horizontal = T)
        
        
        names=c("Management","Business and Financial Operations",
                "Architecture and Engineering Occupation","Life, Physical, and Social Science",
                "Art, Design, Entertainment, Sports, And Media","Healthcare Practitioners and Technical Occupation",
                "Protective Service","Food Preparation and Serving Related",
                "Office and Administrative Support","Farming, Fishing, and Forestry",
                "Construction and Extraction","Installation, Maintenance, and Repair","Production",
                "Transportation and Material Moving"),las=1)



#make onetva table

head(onetva)
onetva<-onetva[,c(3,5,8)]
head(onetva)
onetva<-onetva[!duplicated(onetva[c(1,3)]),]
onetva<-onetva[order(onetva$`confidence_cert`,decreasing=TRUE),]
head(onetva)
onetva<-onetva[,c(1,3)]
head(onetva)
onetva<-onetva[onetva$certificate!="",]
colnames(onetva)<-c("ONET Certification","VA Certification")
write.csv(onetva,"onetva.csv")






#make va bgt table
head(vabgt)
vabgt<-vabgt[,c(3,5,8)]
vabgt<-vabgt[!duplicated(vabgt[c(1,3)]),]
vabgt<-vabgt[order(vabgt$`confidence_cert`,decreasing=TRUE),]
vabgt<-vabgt[,c(1,3)]
head(vabgt)
tail(vabgt)
vabgt<-vabgt[vabgt$certificate!="",]
colnames(vabgt)<-c("VA Certification","BGT Certification")
write.csv(vabgt,"vabgt.csv")
