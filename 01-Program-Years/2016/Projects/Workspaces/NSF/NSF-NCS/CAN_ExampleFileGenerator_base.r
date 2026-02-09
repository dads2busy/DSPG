rm(y,j,o,i,z,n)
u <- 22 #NUMB UNITS IN NET
s <- 1
#s from above
#n here is the number of situations in each set (SAME FOR ALL SETS)
n <- length(ReaganXYZ[,1])
for (i in 1:s) { #IS JUST 1 
  for(j in 1:n) { #IS LENGTH OF FULL DATA SET
    
    y <- paste("name: sit",j,sep="")
    write(as.character(y),ncolumns=1+u+1,file=paste("ReaganInputs.",i,".ex",sep=""),append=TRUE)
    o <- get(paste("d.m1c.f.10.vir.stim.lrn.wgts.use.",i,sep=""))[j,]
    z <- c("B:",o,";")
    write(as.character(z),ncolumns=1+u+1,file=paste("ReaganInputs.",i,".ex",sep=""),append=TRUE)

  }
}

#EOF
