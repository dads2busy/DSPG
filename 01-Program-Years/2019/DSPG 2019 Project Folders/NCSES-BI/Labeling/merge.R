
responses <- read.csv("~/OneDrive - University of Virginia/SDAD Drive/DSPG/2019 DSPG Program/DSPG 2019 Program - shared folder/DSPG 2019 Project Folders/NCSES-BI/Labeling/responses_new.csv",strip.white=TRUE)

head(responses)
dim(responses)

library(dplyr)

data_freq <- data.frame(sort(table(responses$Please.copy.and.paste.the.Document.ID.of.the.article.you.are.submitting.responses.for.),dec=T))

dim(data_freq)

data_freq <- data_freq[which(data_freq$Var1 != ""),]

dim(data_freq)
head(data_freq)

responses <-responses[which(responses$Please.copy.and.paste.the.Document.ID.of.the.article.you.are.submitting.responses.for. !=""),]

data_merged <- merge(responses,data_freq,by.x="Please.copy.and.paste.the.Document.ID.of.the.article.you.are.submitting.responses.for.",by.y="Var1")

data_merged <- data_merged[,c(2,1,9,3:8)]
colnames(data_merged) <- c("Timestamp","ID","Freq","innovationYes","Phase","Drugname","Companyname","new2market","new2company")


#LABELER DATA


df_labeler <- read.csv("~/OneDrive - University of Virginia/SDAD Drive/DSPG/2019 DSPG Program/DSPG 2019 Program - shared folder/DSPG 2019 Project Folders/NCSES-BI/Labeling/df33.csv",strip.white=TRUE)
df_labeler <- df_labeler[,2:3]

dim(df_labeler)
#625

table(df_labeler$labeller)
sort(table(df_labeler$an),dec=T)

length(sort(table(df_labeler$an),dec=T))

#remove Joel's

df_labeler <- df_labeler[which(df_labeler$labeller != "Extra3"),]
dim(df_labeler)
#600

labeler_freq <- data.frame(sort(table(df_labeler$an),dec=T))
dim(labeler_freq)
#420

data_labeler_freq <- merge(labeler_freq,df_labeler,by.x = "Var1",by.y="an",all.x=T)

#Get documents that are not labeled (although allocated)

set_all <- unique(df_labeler$an)

View(data_merged)
set_labeled <- unique(data_merged$ID)

labeled_no_by_default <- setdiff(set_all,set_labeled)
length(labeled_no_by_default)
#79

length(unique(labeled_no_by_default))

#defaul no articlkes and ferquencies and labelers
xx <- data_labeler_freq[which(data_labeler_freq$Var1 %in% labeled_no_by_default),]
#Var1 Freq labeller
dim(xx)
xx$defaultlabel <- "Yes"
#add these to data_merged (labeled dataset) with no as the answer

data_merged$labeller <- "NA"
data_merged$defaultlabel <- "No"
colnames(data_merged)


newdataframe <- data.frame(matrix(, nrow=85, ncol=7))

xx_new <- cbind(xx,newdataframe)

xx_new <- xx_new[,c(5,1,2,6:11,3:4)]

colnames(xx_new) <- colnames(data_merged)

xx_new$innovationYes <- "No_by_Default"

data_merged_with_defaults <- rbind(data_merged,xx_new)


#Get documents allocated only once

allocated_once <- data_labeler_freq[which(data_labeler_freq$Freq == 1),]

data_merged_with_defaults$Allocation_Freq <- "NA"

colnames(allocated_once) <- c("ID","Allocation_Freq","labeller")
dim(allocated_once)
#262

length(unique(allocated_once$ID))
#262
setdiff(allocated_once$ID,data_merged_with_defaults$ID)
#0 goodnews!

dim(data_merged_with_defaults)
#600

data_merged_with_defaults_and_once <- merge(data_merged_with_defaults,allocated_once,by="ID",all.x=T,all.y=T)
dim(data_merged_with_defaults_and_once)

#check when allocated once, frequency more than 1
#clean those

data_merged_with_defaults_and_once <- data_merged_with_defaults_and_once[,c(2,1,3,16,15,4,5,6,7,8,9,11,13)]
dim(data_merged_with_defaults_and_once)
#600

double_labeling_same_person <- data_merged_with_defaults_and_once[which(data_merged_with_defaults_and_once$labeler_allocated_once !="NA" & data_merged_with_defaults_and_once$Freq >1),]
#45 entries
length(unique(double_labeling_same_person$ID))
#21 articles
#check these and clean


#Remaining articles with multiple allocations
data_merged_multiple_allocations <- data_merged_with_defaults_and_once[!(data_merged_with_defaults_and_once$ID %in% double_labeling_same_person$ID),]
dim(data_merged_multiple_allocations)
#555

yy <- data.frame(sort(table(data_merged_multiple_allocations$ID),dec=T))
colnames(yy) <- c("ID","Labeling_Freq")

data_remerged <- merge(data_merged_multiple_allocations,yy,by="ID",all.x=T,all.y=T)
data_remerged <-data_remerged[,c(2,1,3,14,4:13)]
data_remerged <-data_remerged[,c(1,2,4,14,5:13)]
dim(data_remerged)

data_remerged <- data_remerged[which(data_remerged$ID != ""),]
#576


#MERGE THIS WITH LABELING FREQUENCY

data_remerged 

dim(data_labeler_freq)
#612

colnames(data_labeler_freq)

length(unique(data_labeler_freq$Var1))
#420

length(unique(data_remerged$ID))
#435

colnames(data_labeler_freq) <- c("ID","Allocation_Freq","allocated_labeller")

head(data_labeler_freq)

data_labelers_freq <- data_labeler_freq %>%
  group_by(ID) %>%
  mutate(allocated_labellers = paste0(allocated_labeller,collapse="_"))

data_labelers_freq <- unique(data_labelers_freq[,c(1,2,4)] )


finaldataset <- merge(data_remerged,data_labelers_freq,all.x=T,all.y=T,by="ID")
dim(finaldataset)

colnames(finaldataset)
finaldataset <- finaldataset[,c(2,1,3,14,4,15,5,6,7:13)]

dim(finaldataset)
#969

length(unique(finaldataset$ID))
#435

finaldataset <- finaldataset[which(finaldataset$Labeling_Freq != 0),]

#948
dim(finaldataset[which(finaldataset$Labeling_Freq ==1),])
#311
dim(finaldataset[which(finaldataset$Labeling_Freq !=1),])
#244


multiple_labels <- finaldataset[which(finaldataset$Labeling_Freq !=1),]

length(unique(multiple_labels$ID))
#103

#if allocation and labeling freq are equal, and answers are same -> take one (consistent replies)

#if allocation and labeling freq are equal, and answers are different -> label again

#if allocation and labeling freq are not equal, and answers are different -> take the recent answer


