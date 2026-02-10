SOC <- c(12, 12, 12, 13, 13, 14, 15)
cred <- c("a", "b", "c", "c", "d", "d", "e")

length(SOC)
length(cred)

mat <- cbind(cred, SOC)

library(tidyr)

#this is a two-mode network
#affiliation networks
#pg 131 bn.pr$proj1

#I need this to turn into an adjacency matrix or edge-list 
#pivot so that we have a list of all Creds for each SOC code -- 111 rows, up to the max # of credentials

#edge-list -> rows are unique pairs that share a soc code
#after pivoting into wide
#2 for loops
#num(credentials) choose 2

numnodes <- length(cred)
numedges <- length(unique(SOC))

d <- matrix(,1137,4)

cert <- read_xlsx("ONET_SOC_credentials.xlsx")
cert<- data.frame(cbind(cert$Certification.Name, cert$SOC))
cert2 <- cbind(cert,d)
colnames(cert2) <- c("cert", "soc", "soc1", "soc2", "soc3", "soc4")
cert_wide <- pivot_wider(cert2, values_from = soc)
View(cert_wide)

empty_mat <- matrix(,943,943)


#make a loop:
#one column is cert, the second column is a vector of SOC codes that require that credential
#loop through finding the unique(cert), and add to a list of SOC codes

certs_unique <- unique(cert$cert)
#socs = an empty column 
for (certs_unqiue in cert) {
 # socs[i] <- c(socs, soc)
}


certlist<-unique(cert$Certification.Name)
vectorlist<-list()
for(i in 1:length(certlist)){
  vectorlist[[i]]<-c(cert[cert$Certification.Name==certlist[i],4])
}



adjacencyMatrix<-matrix(rep(0,length(certlist)^2),nrow=length(certlist))
for(i in 1:length(certlist)){
  for(j in 1:length(certlist)){
    if(sum(vectorlist[[i]] %in% vectorlist[[j]])>0){
      adjacencyMatrix[i,j]<-1
    } 
  }
}

