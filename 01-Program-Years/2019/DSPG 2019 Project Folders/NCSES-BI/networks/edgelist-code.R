library(data.table)

data <- read.csv("~/OneDrive - University of Virginia/SDAD Drive/DSPG/2019 DSPG Program/DSPG 2019 Program - shared folder/DSPG 2019 Project Folders/NCSES-BI/networks/labeling-clean.csv")

dat <- data[which(data$ID != ""),]

dat <- dat[which(dat$company_codes != "[]"),]
class(dat$company_codes[1])

dat$company_codes <- as.character(dat$company_codes)

dat$company_codes <- gsub("\\[","",dat$company_codes)
dat$company_codes <- gsub("\\]","",dat$company_codes)
dat$company_codes <- gsub("'","",dat$company_codes)


dt <- data.table(dat)

#dt <- data.table(id=c(10, 20), val=c("a,b,c", "c,d,e"))
#print(dt)


dt1 <- dt[, list(company_codes = unlist(strsplit(company_codes, ","))), by=ID]
dt1$company_codes <- gsub(" ","",dt1$company_codes)
dt1 <- unique(dt1)
dt1 <- data.frame(dt1)

sort(table(dt1$ID),dec=T)

xx <- data.frame(sort(table(dt1$ID),dec=T))

IDs <- xx[which(xx$Freq > 1),]$Var1

dt2 <- dt1[which(dt1$ID %in% IDs),]


edge.list <- by(dt2, dt2$ID, function(x) t(combn(as.vector(x$company_codes), 2)))

do.call(rbind, edge.list)


dt2 <- data.table(dt2)



x <- dt2[,combn(company_codes,2),by=ID][,matrix(V1,ncol=2,byrow = T)]

y<-data.table(x)

edges <- y[,.N,by=.(V1,V2)]

write.csv(edges,"~/OneDrive - University of Virginia/SDAD Drive/DSPG/2019 DSPG Program/DSPG 2019 Program - shared folder/DSPG 2019 Project Folders/NCSES-BI/networks/edges_labeling.csv",row.names = F)


ID <- c(1,1,1,2,2,2,3,3)
Category <- c("A","B","C","B","E","H","C","E")
datr <- data.frame(ID,Category)

edge.list <- by(datr, datr$ID, function(x) t(combn(as.vector(x$Category), 2)))

