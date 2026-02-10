# scraping Figshare


#step 1: use the API to gather json file of most reused data sets



#step 2: get urls and ids of those resused data sets from json file
library(jsonlite)
jsonD1 = fromJSON("FigshareTopDownloads1thru1000.txt")
jsonD2 = fromJSON("FigshareTopDownloads1001thru2000.txt")
jsonD3 = fromJSON("FigshareTopDownloads2001thru3000.txt")
jsonD4 = fromJSON("FigshareTopDownloads3001thru4000.txt")
jsonD5 = fromJSON("FigshareTopDownloads4001thru5000.txt")
jsonC1 = fromJSON("FigshareTopCites1thru1000.txt")
jsonC2 = fromJSON("FigshareTopCites1001thru2000.txt")
jsonC3 = fromJSON("FigshareTopCites2001thru3000.txt")
jsonC4 = fromJSON("FigshareTopCites3001thru4000.txt")
jsonC5 = fromJSON("FigshareTopCites4001thru5000.txt")
jsonURLS<-c(jsonD1$url_public_html,jsonD2$url_public_html,jsonD3$url_public_html,jsonD4$url_public_html,jsonD5$url_public_html,
            jsonC1$url_public_html,jsonC2$url_public_html,jsonC3$url_public_html,jsonC4$url_public_html,jsonC5$url_public_html)
jsonURLS<-unique(jsonURLS)



#step 3: write a loop to scrape desired metrics from all of those files. Write a dataset for all pieces of information
library(rvest)
library(RSelenium)
#figshareMetrics<-matrix(rep(-1,length(jsonURLS)*9),nrow=length(jsonURLS))
rD <- rsDriver(browser="firefox", port=4591L, verbose=F)
remDr <- rD[["client"]]
remDr$navigate("https://figshare.com")
for(i in 7671:length(jsonURLS)){
  if(i%%50==0){print(i)}
  url<-jsonURLS[i]
  remDr$navigate(url)
  for (j in 1:20) {
    Sys.sleep(1) # give the page time to fully load
    html <- remDr$getPageSource()[[1]]
    page <- read_html(html)
    
    metrics<-html_nodes(page,"div.fSZ_n")
    if(length(metrics)==0){next}
    downloadSize<-html_nodes(page,"span._2S69P")
    fileNumber<-html_nodes(page,"span._3owtv")
    numberKeywords<-html_nodes(page,"a._2PSMA._1jrLT._3kcSK._3v5nv")
    license<-html_nodes(page,"div.Ueoc2 a")
    history<-html_nodes(page,"span._1qu0d")
    
    if (class(try(any(grepl("\\d",html_text(metrics[1])))&any(grepl("\\d",html_text(metrics[2])))&any(grepl("\\d",html_text(metrics[3])))&
                  any(grepl("\\d",html_text(downloadSize)))&any(grepl("\\d",length(numberKeywords)))&any(grepl("\\d",length(history))), silent = T))=="try-error") {next}
    if (any(grepl("\\d",html_text(metrics[1])))&any(grepl("\\d",html_text(metrics[2])))&any(grepl("\\d",html_text(metrics[3])))&
        any(grepl("\\d",html_text(downloadSize)))&any(grepl("\\d",length(numberKeywords)))&any(grepl("\\d",length(history)))
        ) {break}
    
    
  }
  if(length(metrics)==0){next} #if link is broken
  if(j==20){next}
  if(length(fileNumber)==0){fileNumberText<-"1 file"}
  else{fileNumberText<-html_text(fileNumber)}
  if(is.na(html_text(license))){licenseText<-"Not found"}
  else{licenseText<-html_text(license)}
  if (class(try(figshareMetrics[i,]<-c(url,html_text(metrics),html_text(downloadSize),fileNumberText,length(numberKeywords),licenseText,length(history)), silent = T))=="try-error") {next}
  figshareMetrics[i,]<-c(url,html_text(metrics),html_text(downloadSize),fileNumberText,length(numberKeywords),licenseText,length(history))
}
figshareMetricsSave<-data.frame(figshareMetrics)
colnames(figshareMetricsSave)<-c("url","Views","Downloads","Citations","Size","NumberOfFiles","NumberOfKeywords",
                             "License","TotalEdits")
write.csv(figshareMetricsSave,"figshare.csv")












# analyzing figshare
library(corrplot)
data<-read.csv("figshareSAVE.csv",header=TRUE)
head(data)
data<-data[,c(2:5,8:11,13)]
downloadsmod<-lm(data$Downloads~data$Views+data$NumberOfFiles+data$NumberOfKeywords+data$SizeStandard+as.factor(data$License))
summary(downloadsmod)
summary(data$NumberOfKeywords)
data$KeywordBin[data$NumberOfKeywords<4]<-"Group 1: Up to 3"
data$KeywordBin[data$NumberOfKeywords<6&data$NumberOfKeywords>3]<-"Group 2: 4 to 5"
data$KeywordBin[data$NumberOfKeywords<9&data$NumberOfKeywords>5]<-"Group 3: 6 to 8"
data$KeywordBin[data$NumberOfKeywords>8]<-"Group 4: 9 or more"
barplot(aggregate(Downloads~KeywordBin,data,mean)$Downloads, names.arg=c("Up to 3","4 to 5","6 to 8","9 or more"),
        main="Total Downloads by Number of Keywords",xlab="Keyword Quartile",ylab="Downloads")

keywordsframe<-aggregate(Downloads~KeywordBin,data,mean)$Downloads
keywordsframe<-as.data.frame(keywordsframe)
colnames(keywordsframe)<-"Downloads"
wordbins<-c("1. Up to 3","2. 4 to 5","3. 6 to 8","4. 9 or more")
wordbins<-as.data.frame(wordbins)
colnames(wordbins)<-"Group"
figkeys<-cbind(wordbins,keywordsframe)


ggplot(data=figkeys, aes(x=Group,y=Downloads))+
       geom_bar(stat="identity",fill="#E57200")+
       geom_text(aes(label=round(Downloads)),color="black",vjust=0,size=4)+
       ggtitle("Average Total Downloads Per Dataset by Number of Keywords") +
       xlab("Keywords") + ylab("Downloads") +
       scale_x_discrete(labels=c("Up to 3","4 to 5","6 to 8","9 or more")) +
       theme(legend.position = "none",
         plot.title = element_text(hjust=0.5,face="bold",size=15),
         axis.text.x=element_text(color = "black", size=9, angle=35, vjust=.8, hjust=0.8),
         axis.title.x = element_text(size=12),
         axis.title.y = element_text(size=12)
  )



#license boxplots
boxplots<-data[,c(3,7)]
boxplots<-boxplots[grep("CC",data$License),]
boxplots$License<-gsub(" 4.0","",boxplots$License)
boxplots$License<-gsub(" 3.0","",boxplots$License)
boxplots$License<-gsub(" 1.0","",boxplots$License)
table(boxplots$License)
#license downloads
ggplot(boxplots,aes(x=License,y=Downloads))+
  geom_boxplot(fill = "#E57200")+
  ylim(0,2500)+
  ggtitle("Downloads by Creative Commons License") +
  xlab("License") + ylab("Number of Downloads") + theme(plot.title = element_text(hjust = 0.5))

#license pie chart
#ggplot(boxplots,aes(x="",y=License))+
#  geom_bar(stat="identity", width=1) +
#  coord_polar("y", start=0)
slices<-table(boxplots$License)       
lbls<-c("CC BY","CC BY-NC","CC BY-NC-ND","CC BY-NC-SA","CC BY-ND","CC BY-SA","CC BY + CC0","CC0")
pie(slices,lbls,col=rainbow(length(lbls)),main="Creative Commons Chosen Licenses in Figshare Sample")
#break down all small six to "other"
slices<-c(sum(slices[2:7]),slices[1],slices[8])
lbls<-c("CC BY","Other","CC0")
pie(slices,lbls,col=c("#E57200","green","#232D4B"),main="Creative Commons Chosen Licenses in Figshare Sample")


#collapse further
boxplots$License[(boxplots$License!="CC BY")&(boxplots$License!="CC0")]<-"Other"
ggplot(boxplots,aes(x=License,y=Downloads))+
  geom_boxplot(fill = "#E57200")+
  ylim(0,2500)+
  ggtitle("Downloads by Creative Commons License") +
  xlab("License") + ylab("Number of Downloads") + theme(plot.title = element_text(hjust = 0.5))




licensetest<-aov(Downloads~as.factor(License),data=data)
summary(licensetest)

corrplot(cor(data[,2:4]))














# analyzing nsf
nsf<-read.csv("NSF.csv",header=TRUE)
nsf<-nsf[,1:14]
library(predictrace)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(RColorBrewer)
head(nsf)
colnames(nsf)
nsf$TITLE<-gsub('[[:digit:]]+', '', nsf$TITLE)
nsf_title_words <- nsf %>% unnest_tokens(word,TITLE) %>% anti_join(stop_words) %>% count(word,sort=T)
nsf_title_words<-nsf_title_words[-1,]
set.seed("123")
wordcloud(words=nsf_title_words$word,freq=nsf_title_words$n,min.freq = 1,
          max.words=100,random.order=FALSE,rot.per=0.35,colors=brewer.pal(8,"Dark2"))
nsfyears<-barplot(table(nsf$YEAR),main="Number of Published Items by Year Since 2014",ylab="Published Items",xlab="Year",ylim=c(0,4000))
text(nsfyears,table(nsf$YEAR)+100,table(nsf$YEAR))


nsfyears<-as.data.frame(table(nsf$YEAR))

ggplot(data=nsfyears, aes(x=Var1,y=Freq))+
  geom_bar(stat="identity",fill="#E57200")+
  geom_text(aes(label=Freq),color="black",vjust=0,size=4)+
  ggtitle("Number of Published Items by Year") +
  xlab("Year") + ylab("Published Items") +
  #scale_x_discrete(labels=c("Up to 3","4 to 5","6 to 8","9 or more")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5,face="bold",size=15),
        axis.text.x=element_text(color = "black", size=9, angle=35, vjust=.8, hjust=0.8),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12)
  )











#now look at researchers
library(stringr)
authors<-nsf$AUTHORS
authors<-gsub('[[:digit:]]+', '', authors)
authors<-gsub("ORCID:","",authors)
write.csv(authors,"nsf_authors.csv")
#then go in and get rid of ;s
authors<-read.csv("nsf_authors.csv",header=FALSE)
authors<-as.vector(authors)
authors<-unlist(authors)
authors<-as.data.frame(authors)
authors<-authors[authors!=""]
authors<-as.data.frame(authors)
colnames(authors)<-"Author"
authors$first<-str_split_fixed(authors$Author,",",2)[,2]
authors$last<-str_split_fixed(authors$Author,",",2)[,1]
authors$first<-sub(" .*", "", authors$first)
races<-predict_race(authors$last,probability = F)
summary(as.factor(races$likely_race))
genders<-predict_gender(authors$first,probability=F)
summary(as.factor(genders$likely_gender))
slices <- summary(as.factor(genders$likely_gender))
lbls <- c("Female","Female/Male","Male","Unknown")
pie(slices, labels = lbls, main="NSF PAR Authors' Gender Breakdown",col=brewer.pal(8,"Dark2"))










#aditi test
library(rvest)
library(RSelenium)
rD <- rsDriver(browser="firefox", port=4591L, verbose=F)
remDr <- rD[["client"]]
remDr$navigate("https://search.dataone.org/data")
remDr$navigate(url)
Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]
site <- read_html(html)
#put in code here to collect info like you would from first page
for(i in 1:100){
  remDr$findElements("css selector", paste0("a.pagerLink[page='",2,"']"))[[1]]$clickElement()
  #put your code in here too. you can probably use the same stuff
}













