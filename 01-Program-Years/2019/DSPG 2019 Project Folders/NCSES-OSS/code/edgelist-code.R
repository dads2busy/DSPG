ID <- c(1,1,1,2,2,2,3,3,3)
Category <- c("A","B","C","B","E","H","C","E","A")
dat <- data.frame(ID,Category)

edge.list <- by(dat, dat$ID, function(x) t(combn(as.vector(x$Category), 2)))

do.call(rbind, edge.list)



library(data.table)
dat<-data.table(dat)

x <- dat[,combn(Category,2),by=ID][,matrix(V1,ncol=2,byrow = T)]

y<-data.table(x)

z <- y[,.N,by=.(V1,V2)]

colnames(z) <- c("Source","Target","weight")
class(z)

graph_from_data_frame(d, directed = TRUE, vertices = NULL)

library(igraph)
?igraph

gr <- graph_from_data_frame(z, directed = FALSE, vertices = NULL)

print(gr, e=TRUE, v=TRUE)
degree(gr)

E(gr)$weight