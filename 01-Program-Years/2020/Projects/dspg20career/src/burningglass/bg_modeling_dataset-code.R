require(MCMCpack)
require(parallel)
require(tidyverse)
# ARI 3: BGT Data
# Joanna :: Start here
########################
# Intercept only model #
########################
data <- read_csv("~/git/DSPG2020/career/data/modeling_dataset_states.csv")
data <- data.frame(data)
t <- ncol(data[,-1]) # exclude id
states <- apply(data[,-1],2,unique)[(order(apply(data[,-1],2,unique)[,1])),1]; states.l <- length(states)
data.lst <- list()
for(i in 1:nrow(data)) data.lst[[i]] <- match(data[i,-1],states)
data.vec <- t(do.call(cbind,data.lst))
parallel.index <- 1:states.l
numCores <- parallel::detectCores()
system.time(est.results <- parallel::mclapply(parallel.index, function(x){
  y <- c()
  for(i.t in 2:t) y <- c(y,data.vec[data.vec[,(i.t-1)]==x,i.t])
  y <- y[!is.na(y)]
  
  model <- MCMCpack::MCMCmnl(y~1,baseline="1",mcmc.method = "IndMH",
                             verbose=500, mcmc=1e4, thin=10, tune=0.5)
  m.summary <- summary(model)
  cbind(m.summary$statistics,coda::HPDinterval(model))
}, mc.cores=numCores))

p.est <- do.call(rbind,lapply(est.results, function(x){
  p.fit <- exp(x[,"Mean"])/(1+sum(exp(x[,"Mean"])))
  p.fit <- c(1-sum(p.fit),p.fit)
  p.fit
}))
colnames(p.est) <- rownames(p.est) <- states
# xtable::xtable(round(p.est,4),auto=T)
est.coef <- round(do.call(rbind,est.results),4)
rownames(est.coef) <- apply(expand.grid(c("Zone 1", "Zone 2", "Zone 3","Zone 4", "Zone 5"),
                                  c("(Education)","(Zone 1)", "(Zone 2)", "(Zone 3)","(Zone 4)", "(Zone 5)"))[,c(2,1)],1, function(x) paste(x[1],x[2],sep=" "))
#xtable::xtable(est.coef[,c(1,5,6)], auto=T)
## Joanna :: Stop here
############################################
# Checking for temporal variation in p.est #
############################################
p.est.t <- sapply(1:(t-1), function(x.t){
  est.results <- parallel::mclapply(parallel.index, function(x){
    y <- c()
    for(i.t in (1+x.t):t) y <- c(y,data.vec[data.vec[,(i.t-x.t)]==x,i.t])
    y <- y[!is.na(y)]
    
    model <- MCMCpack::MCMCmnl(y~1,baseline="1",mcmc.method = "IndMH",
                               verbose=500, mcmc=1e4, thin=10, tune=0.5)
    m.summary <- summary(model)
    cbind(m.summary$statistics,coda::HPDinterval(model))
  }, mc.cores=numCores)
  
  p.est <- do.call(rbind,lapply(est.results, function(x){
    p.fit <- exp(x[,"Mean"])/(1+sum(exp(x[,"Mean"])))
    p.fit <- c(1-sum(p.fit),p.fit)
    p.fit
  }))
  colnames(p.est) <- rownames(p.est) <- states
  b0.est <- lapply(est.results, function(x){
    x[,"Mean"]
  })
  
  b0.est <- do.call(rbind,b0.est)
  colnames(b0.est) <- states[-1]
  rownames(b0.est) <- states
  round(b0.est,4)
  
  list(b0=b0.est, p=p.est)})
# t-step Transition probabilities
col.seq <- colorRampPalette(viridis::magma(8))(36)
expr.list <- apply(expand.grid(1:6,1:6)[,c(2,1)], 1, function(x){
  latex2exp::TeX(paste("p_{",x[1],x[2],"}(t)", sep=""))
})
p.mat <- c()
for(i in 1:ncol(p.est.t)) p.mat <- cbind(p.mat, as.vector(p.est.t["p",i]$p))
plot(p.mat[1,], type="b", ylim=c(0,1), ylab=latex2exp::TeX("$p_{jk}(t)$"), xlab="t (in years)", xlim=c(1,t), col=col.seq[1], pch=16)
for(i in 2:nrow(p.mat)) lines(p.mat[i,], type="b", col=col.seq[i],pch=16)
for(i in 1:nrow(p.mat)) text(x=t-1/2,y=p.mat[i,(t-1)],expr.list[[i]], cex=0.5, col=col.seq[i])
grid()


expr.list <- apply(expand.grid(1:6,2:6)[,c(2,1)], 1, function(x){
  latex2exp::TeX(paste("$\\beta_{0",x[1],x[2],"}(t)$", sep=""))
})
b.mat <- c()
for(i in 1:ncol(p.est.t)) b.mat <- cbind(b.mat, as.vector(p.est.t["b0",i]$b0))
plot(b.mat[1,], type="b", ylim=c(-7.5,5), ylab=latex2exp::TeX("$\\beta_{0jk}(t)$"), xlab="t (in years)", xlim=c(1,t), col=col.seq[1], pch=16)
for(i in 2:nrow(b.mat)) lines(b.mat[i,], type="b", col=col.seq[i],pch=16)
for(i in 1:nrow(b.mat)) text(x=t-1/2,y=b.mat[i,(t-1)],expr.list[[i]], cex=0.5, col=col.seq[i])
grid()

# Testing if transition probability matrices are constant over time
p.mat <- sapply(2:t, function(x){
  crosstab <- matrix(ftable(data.vec[,(x-1)],data.vec[,x]),nr=6,nc=6)
  colnames(crosstab) <- rownames(crosstab) <- c("e",1:5)
  crosstab/rowSums(crosstab)
})

t(apply(p.mat,1,function(x){
  acf.vals <- acf(x,demean = F,plot = F)
  acf.vals$acf[-1]}))

expr.list <- apply(expand.grid(c("e",1:5),c("e",1:5))[,c(2,1)], 1, function(x){
  latex2exp::TeX(paste("p_{",x[1],x[2],"}(t)", sep=""))
})

plot(p.mat[1,], type="b", ylim=c(0,1), ylab=latex2exp::TeX("$p_{jk}(t)$"), xlab="t (in years)", xlim=c(1,t), col=col.seq[1], pch=16)
for(i in 2:nrow(p.mat)) lines(p.mat[i,], type="b", col=col.seq[i],pch=16)
for(i in 1:nrow(p.mat)) text(x=t-1/2,y=p.mat[i,(t-1)],expr.list[[i]], cex=0.5, col=col.seq[i])


pdf("/Users/aritrahader/Box/ARI3-Career Progression and Talent Management/working/plots/p-t.pdf")
mat <- matrix(c(2,3,4,1,
                5,6,7,1,
                8,9,10,1), nr=3,nc=4, byrow=T)
layout(mat,
       widths = c(3,3,3,1.5),
       heights = c(3,3,3))

legend_image <- as.raster(matrix(colorRampPalette(RColorBrewer::brewer.pal(11,"Spectral"))(20), ncol=1))
plot(c(0,3),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
text(x=2, y = seq(0.01,0.99,l=6), labels = round(seq(0,1,l=6),2))
rasterImage(legend_image, 0, 0, 1,1)

for(i in 1:9){
  plot(p.mat[[i]],
       col = rev(colorRampPalette(RColorBrewer::brewer.pal(11,"Spectral"))(20)),
       main="",key=NULL, xlab="", ylab="")
  axis(3,at=1:6,labels = c("e",1:5))
  axis(4,at=1:6,labels = rev(c("e",1:5)))
  title(paste("TPM (",i,"-",i+1,")",sep=""),line = +2)
}
dev.off()
#############################################
# Exploring spatial and temporal dependence #
#############################################
data <- readxl::read_xlsx("/Users/aritrahader/Box/ARI3-Career Progression and Talent Management/bg_modeling_dataset.xlsx",
                          sheet=3)
cpath <- readxl::read_xlsx("/Users/aritrahader/Box/ARI3-Career Progression and Talent Management/bg_modeling_dataset.xlsx",
                           sheet=2)
nyears <- ncol(cpath[,-1])
states <- apply(cpath[,-1],2,unique)[(order(apply(cpath[,-1],2,unique)[,1])),1]; states.l <- length(states)
data.cpath <- merge(data,cbind.data.frame(id=cpath$id,apply(cpath[,-1],2,function(x) match(x,states))),by="id",all=T)

usa_shape <- readRDS("/Users/aritrahader/gadm36_USA_2_sp.rds")
shape <- subset(usa_shape,NAME_1=="Virginia" | NAME_1 =="District of Columbia" | NAME_1 =="Maryland"| NAME_1 == "Delaware")
pdf("/Users/aritrahader/Box/ARI3-Career Progression and Talent Management/working/plots/spatial-plots.pdf",width = 12, height=8)
for(i in 1:10){
  zip_choice <- cbind.data.frame(zipcode[match(data.cpath$zip,zipcode$zip),c("longitude","latitude")],
                                 zip=unique(data.cpath$zipcode),
                                 choice=sapply(unique(data.cpath$zipcode), function(x){
                                   names(which.max(table(data.cpath[data.cpath$zipcode==x,as.character(i)])))
                                 }))
  rownames(zip_choice) <- NULL
  zip_choice$choice_num <- match(zip_choice$choice,states)
  sp_plot(col.seq.length = 6,
          col.text = "Set2",
          data_frame = cbind.data.frame(zip_choice$longitude,zip_choice$latitude,zip_choice$choice_num),
          shape = shape, categorical=T)
  title(main=paste("Year",i,sep="-"))
}
dev.off()


##############################################
# Bayesian MNL Regression with covariates    #
##############################################
require(parallel)
require(MCMCpack)
data.cpath <- data.cpath[!is.na(data.cpath$irr),]
data.cpath <- data.cpath[-which(data.cpath$gender=="NA"),]
# create the design matrix
Xmat <- cbind(data.cpath$irr,
              as.factor(data.cpath$officer_status),
              as.factor(data.cpath$gender))

colnames(Xmat) <- c("irr","officer_status","gender")

rownames(Xmat) <- data.cpath$id
cpath <- data.cpath[,-c(1:ncol(data))]
parallel.index <- 1:(states.l)
numCores <- detectCores()
system.time(est.results <- mclapply(parallel.index, function(x){
  y <- c(); X <- c()
  for(i.t in 2:nyears){
    y <- c(y,cpath[cpath[,(i.t-1)]==x,i.t])
    X <- rbind(X,Xmat[cpath[,(i.t-1)]==x,])
  }
  # check for na
  X <- X[!is.na(y),]; y <- y[!is.na(y)]
  # check tuning parameter and trace plot before inferring::
  # use plot(model), posteriors should look like normal
  # no double modes
  model <- MCMCmnl(y~X-1, baseline="1",mcmc.method = "IndMH",
                   verbose=500, mcmc=5e3, thin=1, tune=1)
  m.summary <- summary(model)
  cbind(m.summary$statistics[,1],HPDinterval(model))
}, mc.cores=numCores))

est.coef <- est.hpd.lower <- est.hpd.upper <-  array(NA,dim=c(states.l,states.l-1,ncol(Xmat)))
for(i in 1:length(est.results)){
  id.est.coef <- split(1:((states.l-1)*ncol(Xmat)),ceiling(seq_along(1:((states.l-1)*ncol(Xmat)))/(states.l-1)))
  for(j in 1:length(id.est.coef)){
    est.coef[i,,j] <- est.results[[i]][id.est.coef[[j]],1]
    est.hpd.lower[i,,j] <- est.results[[i]][id.est.coef[[j]],2]
    est.hpd.upper[i,,j] <- est.results[[i]][id.est.coef[[j]],3]
  }
}
# Estimated Transition Probability Matrix
tpm.mc.est <- t(sapply(1:(states.l),function(x){
  p.x <- sapply(1:(states.l-1), function(y){
    X <- c(); y.st <- c();
    for(i.t in 2:nyears){
      y.st <- c(y.st,cpath[cpath[,(i.t-1)]==x,i.t])
      X <- rbind(X,Xmat[cpath[,(i.t-1)]==x,])
    } 
    exp(crossprod(t(X[!is.na(y.st),]),est.coef[x,y,]))
  })
  p.x <- t(apply(p.x, 1, function(z) c(1,z)/(1+sum(z))))
  # MLE for multinomial probability
  colSums(t(apply(p.x,1,rmultinom, n=1, size=1)))/nrow(p.x)
}))

colnames(tpm.mc.est) <- rownames(tpm.mc.est) <- states
xtable::xtable(round(tpm.mc.est,4), auto=T)
lapply(est.results,)
round(do.call(rbind,est.results),4)
xtable::xtable(round(do.call(rbind,est.results),4), auto=T)

##############################################
# GAM MNL: Spline for year_entered_workforce #
##############################################
require(splines)

# create the design matrix
Xmat <- cbind(data.cpath$irr,
              as.factor(data.cpath$officer_status),
              as.factor(data.cpath$gender),
              data.cpath$year_entered_workforce)

colnames(Xmat) <- c("irr","officer_status","gender","year_entered_workforce")

rownames(Xmat) <- data.cpath$id
cpath <- data.cpath[,-c(1:ncol(data))]
parallel.index <- 1:(states.l)
numCores <- detectCores()
system.time(est.results <- mclapply(parallel.index, function(x){
  y <- c(); X <- c()
  for(i.t in 2:nyears){
    y <- c(y,cpath[cpath[,(i.t-1)]==x,i.t])
    X <- rbind(X,Xmat[cpath[,(i.t-1)]==x,])
  }
  # check for na
  X <- X[!is.na(y),]; y <- y[!is.na(y)]
  if(x==3){
    # linear spline on year for state 3
    year_sp <- bs(X[,"year_entered_workforce"], degree=1)
    colnames(year_sp) <- paste("year_sp",1:ncol(year_sp),sep="")
    X <- X[,-4]
    X <- cbind(X,year_sp)
  }else{
    # cubic spline with degree 3 for year
    year_sp <- bs(X[,"year_entered_workforce"], degree=3)
    colnames(year_sp) <- paste("year_sp",1:ncol(year_sp),sep="")
    X <- X[,-4]
    X <- cbind(X,year_sp) 
  }
  # check tuning parameter and trace plot before inferring::
  # use plot(model), posteriors should look like normal
  # no double modes
  model <- MCMCmnl(y~X-1, baseline="1",mcmc.method = "IndMH",
                   verbose=500, mcmc=5e3, thin=1, tune=1)
  m.summary <- summary(model)
  cbind(m.summary$statistics[,1],HPDinterval(model))
}, mc.cores=numCores))

est.coef <- est.hpd.lower <- est.hpd.upper <-  array(NA,dim=c(states.l,states.l-1,ncol(Xmat)+2))
for(i in 1:length(est.results)){
  id.est.coef <- split(1:((states.l-1)*(ncol(Xmat)+2)),ceiling(seq_along(1:((states.l-1)*(ncol(Xmat)+2)))/(states.l-1)))
  if(i==3){
    for(j in 1:4){
      est.coef[i,,j] <- est.results[[i]][id.est.coef[[j]],1]
      est.hpd.lower[i,,j] <- est.results[[i]][id.est.coef[[j]],2]
      est.hpd.upper[i,,j] <- est.results[[i]][id.est.coef[[j]],3]
    } 
  }else{
    for(j in 1:length(id.est.coef)){
      est.coef[i,,j] <- est.results[[i]][id.est.coef[[j]],1]
      est.hpd.lower[i,,j] <- est.results[[i]][id.est.coef[[j]],2]
      est.hpd.upper[i,,j] <- est.results[[i]][id.est.coef[[j]],3]
    } 
  }
}
# Estimated Transition Probability Matrix
tpm.mc.est <- t(sapply(1:(states.l),function(x){
  if(x==3){
    p.x <- sapply(1:(states.l-1), function(y){
      X <- c(); y.st <- c();
      for(i.t in 2:nyears){
        y.st <- c(y.st,cpath[cpath[,(i.t-1)]==x,i.t])
        X <- rbind(X,Xmat[cpath[,(i.t-1)]==x,])
      } 
      year_sp <- bs(X[,"year_entered_workforce"], degree=1)
      colnames(year_sp) <- paste("year_sp",1:ncol(year_sp),sep="")
      X <- X[,-4]
      X <- cbind(X,year_sp)
      exp(crossprod(t(X[!is.na(y.st),]),na.exclude(est.coef[x,y,])))
    })
  }else{
    p.x <- sapply(1:(states.l-1), function(y){
      X <- c(); y.st <- c();
      for(i.t in 2:nyears){
        y.st <- c(y.st,cpath[cpath[,(i.t-1)]==x,i.t])
        X <- rbind(X,Xmat[cpath[,(i.t-1)]==x,])
      } 
      year_sp <- bs(X[,"year_entered_workforce"], degree=3)
      colnames(year_sp) <- paste("year_sp",1:ncol(year_sp),sep="")
      X <- X[,-4]
      X <- cbind(X,year_sp)
      exp(crossprod(t(X[!is.na(y.st),]),est.coef[x,y,]))
    })
  }
  p.x <- t(apply(p.x, 1, function(z) c(1,z)/(1+sum(z))))
  # MLE for multinomial probability
  colSums(t(apply(p.x,1,rmultinom, n=1, size=1)))/nrow(p.x)
}))

colnames(tpm.mc.est) <- rownames(tpm.mc.est) <- states
xtable::xtable(round(tpm.mc.est,4), auto=T)
round(do.call(rbind,est.results),4)
xtable::xtable(round(do.call(rbind,est.results),4), auto=T)