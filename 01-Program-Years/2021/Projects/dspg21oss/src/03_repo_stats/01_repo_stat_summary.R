library(RPostgres)
library(data.table)
library(tidytable)
library(ggplot2)
library(stringr)
library(stargazer)
library(wordcloud2)
#library(dplyr)

repo_stats<-fread("/sfs/qumulo/qhome/sz3wr/git/dspg21oss/data/dspg21oss/repo_stats_0707.csv")
repo_stats_mean<-repo_stats %>% filter.(stars<=mean(repo_stats$stars), watchers<=mean(repo_stats$watchers), forks<=mean(repo_stats$forks))
repo_stats_topic<-repo_stats %>% filter.(topics!="[]")

# quick stats
summary(repo_stats %>% select.(stars, watchers, forks))

summary(repo_stats_topic %>% select.(stars, watchers, forks))
#might be a good way to display summary stats?
stargazer(repo_stats %>% select.(stars, watchers, forks), median=TRUE)

# some visuals
#single variable histograms
ggplot(repo_stats, aes(x=stars))+geom_histogram()
ggplot(repo_stats, aes(x=watchers))+geom_histogram()
ggplot(repo_stats, aes(x=forks))+geom_histogram()

#two variable scatterplots
ggplot(repo_stats, aes(stars, watchers))+geom_point()+ggtitle("Stars vs. Watchers")+theme_classic()
ggplot(repo_stats, aes(stars, forks))+geom_point()+ggtitle("Stars vs. Forks")+theme_classic()
ggplot(repo_stats, aes(watchers, forks))+geom_point()+ggtitle("Watchers vs. Forks")+theme_classic()

ggplot(repo_stats_mean, aes(stars, watchers))+geom_point()
ggplot(repo_stats_mean, aes(stars, forks))+geom_point()
ggplot(repo_stats_mean, aes(watchers, forks))+geom_point()

#CoLoRs (this is terrible)
ggplot(repo_stats, aes(x=stars, y=forks, fill=watchers))+geom_point()

# keywords
To_List<-function(string){
  temp<-str_replace_all(string, "[\\[\\s\\]]", "")
  temp<-str_split(temp, ",")
  
  return(temp)
}

repo_stats_topic$topics<-map.(repo_stats_topic$topics,To_List)
repo_stats_topic$topics<-map.(repo_stats_topic$topics, unlist)

topic_list<-as.data.table(unlist(repo_stats_topic$topics, recursive = TRUE))
freq<-as.data.table(table(topic_list)) 
freq<-freq %>% arrange.(freq, desc(N))
head(freq, n=12)
colnames(freq)<-c("term", "frequency")
fwrite(freq, "/sfs/qumulo/qhome/sz3wr/git/dspg21oss/data/dspg21oss/topic_frequency.csv")

#freq_temp<-filter.(freq, N>1) %>% arrange.(freq_temp, desc(N))

#viz: word cloud
set.seed(5678)

cloud<-brewer.pal(10, "Paired")[2:12]

wordcloud(words=freq$topic_list, freq=freq$N, min.freq=10, max.words=150, random.color=FALSE,
          random.order=FALSE, rot.per = .3, colors=cloud, scale = c(3, .4))

#wordcloud2(freq)

# popular repos + keywords
repo_stats_popular<-repo_stats %>% filter.(stars>=mean(repo_stats$stars), watchers>=mean(repo_stats$watchers), forks>=mean(repo_stats$forks)) #cutoff based on means
repo_stats_popular$topics<-map.(repo_stats_popular$topics, To_List)

topic_list_popular<-as.data.table(unlist(repo_stats_popular$topics, recursive=TRUE))
freq_popular<-as.data.table(table(topic_list_popular)) %>% arrange.(desc(N))
head(freq_popular, n=12)

#keywords that occur in the same list the most frequently? UPDATE: this crashed RStudio--look into later
for(x in 1:length(repo_stats_topic$topics)){
  if(length(repo_stats_topic$topics[[x]])>1){
    repo_stats_topic$pairs[[x]]<-combn(repo_stats_topic$topics[[x]], 2)
  }
  else{
    repo_stats_topic$pairs[[x]]<-array(c(repo_stats_topic$topics[[x]], " "), dim=c(1, 2))
  }
}

pairs<-as.data.table(repo_stats_topic$pairs) %>% transpose()
pairs_2<-as.data.table(table(as.character(interaction(pairs))))
