########################
# Intercept Only Model #
########################
# set.seed(1234)
require(MCMCpack)
require(parallel)
# Markov Decision Process
# Run First
run_mc <- function(tpm = NULL, state.init = NULL, niter = NULL){
  chain <-  rep(NA,niter)
  chain[1] <- state.init
  for(t in 2:niter){
    p <- tpm[chain[t-1],]
    chain[t] <- which(rmultinom(1,1,p)==1)
    #if(chain[t] == ncol(tpm)) break
  }
  return(as.vector(na.exclude(chain)))
}
states.l <- 6 # change this to 6
states <- c(1:6)
states <- c("Education", "Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5")
#states <- paste("S", c(1:states.l), sep="") 
# change names to BGT data job names

#############################
# Synthetic process: Type 1 #
#############################
# Only intercept
means <- est.coef[,1]
b0 <- rnorm((states.l-1)^2,means,1)
#b0 <- rnorm((states.l-1)^2,0,1) 
# Joanna:: to produce synthetic BGT data:: this needs to be altered
# First steps::
# change the location (mean of normal distribution above) to conform to BGT data estimated intercepts
# keep the scale (sd of normal distribution above) as 1 for now
b0mat <- matrix(b0, nc=(states.l-1),nr=(states.l-1))

colnames(p.est) <- 1:6
rownames(p.est) <- 1:6

N <- nrow(data) # set to the number of rows in BGT data
data.1 <- data %>% dplyr::select(-X1) %>% mutate(init = case_when(X1.1 == "Education" ~ 1,
                                           X1.1 == "Zone 1" ~ 2,
                                           X1.1 == "Zone 2" ~ 3,
                                           X1.1 == "Zone 3" ~ 4,
                                           X1.1 == "Zone 4" ~ 5, 
                                           X1.1 == "Zone 5" ~ 6)) %>% pull(init)
nyears <- 10 # number of years in the BGT data
mc_sim <- sapply(data.1, run_mc, tpm=p.est, niter=nyears, simplify = F) # generates synthetic data

## CALCULATING ERROR ------------------
library(Metrics)

sim_data <- data.frame(matrix(unlist(mc_sim), nrow=length(mc_sim), byrow=TRUE))
data <- data %>% dplyr::select(-X1) %>% mutate_at(vars(X1.1:X10), funs(recode(., "Education" = 1, "Zone 1" = 2, "Zone 2" = 3, "Zone 3" = 4, "Zone 4" = 5, "Zone 5" = 6)))
colnames(data) <- 1:10
colnames(sim_data) <- 1:10

errors <- NULL
for(i in 1:nrow(data)) {
  actual = as.numeric(data[i,])
  predicted = as.numeric(sim_data[i,])
  error <- rmse(actual, predicted)
  errors <- append(errors, error)
}
mean(errors)

#e_error <- NULL
#errors <- NULL
#for(i in 1:nrow(data)) {
#  actual = as.numeric(data[i,])
#  predicted = as.numeric(sim_data[i,])
#  for(e in 1:10) {
#    error <- (predicted[e] - actual[e])^2
#    e_error <- append(e_error, error)
#    e_error <- mean(e_error)
#  } 
#  errors <- append(errors, e_error)
#}
#mean(errors)

#### SAMPLING FROM STDEV ----------------------
errors <- NULL
errors_10 <- NULL
for(t in 1:10){
# Pulling means and stdevs, selecting a random value within stdev
coefs <- as.data.frame(est.coef)
SDvalues <- coefs %>% rowwise() %>% mutate(Value = runif(1, min = (Mean - SD), max = (Mean + SD))) %>% ungroup() %>% dplyr::select(Value) %>% mutate(group = as.factor(c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5), rep(5, 5), rep(6, 5))))

values <- SDvalues %>% pull(Value) 
SDvalues <- split(values, SDvalues$group)

# Creating transition probabilites for random stdev values
random_p.est <- do.call(rbind,lapply(SDvalues, function(x){
  p.fit <- exp(x)/(1+sum(exp(x)))
  p.fit <- c(1-sum(p.fit),p.fit)
  p.fit
}))

# Simulating data using new transition probabilites
mc_sim <- sapply(data.1, run_mc, tpm=random_p.est, niter=nyears, simplify = F)

sim_data <- data.frame(matrix(unlist(mc_sim), nrow=length(mc_sim), byrow=TRUE))
colnames(sim_data) <- 1:10

# Calculating rmse for simulated data
for(i in 1:nrow(data)) {
  actual = as.numeric(data[i,])
  predicted = as.numeric(sim_data[i,])
  error <- rmse(actual, predicted)
  errors <- append(errors, error)
}
mean_error <- mean(errors)
errors_10 <- append(errors_10, mean_error)
}
mean(errors_10)



##### ------------
ids <- data$X1
data <- data[,-1]
sim_data <- data.frame(matrix(unlist(mc_sim), nrow=length(mc_sim), byrow=TRUE))
sim_data <- sim_data %>% mutate_at(vars(X1:X10), funs(recode(., `1` = "Education", `2` = "Zone 1", `3` = "Zone 2", `4` = "Zone 3", `5` = "Zone 4", `6` = "Zone 5")))
data$id <- ids
sim_data$id <- ids

data %>% left_join(sim_data, by = "id") %>% mutate(
  Y1 = ifelse(`1` == X1, 1, 0),
  Y2 = ifelse(`2` == X2, 1, 0),
  Y3 = ifelse(`3` == X3, 1, 0),
  Y4 = ifelse(`4` == X4, 1, 0),
  Y5 = ifelse(`5` == X5, 1, 0),
  Y6 = ifelse(`6` == X6, 1, 0),
  Y7 = ifelse(`7` == X7, 1, 0),
  Y8 = ifelse(`8` == X8, 1, 0),
  Y9 = ifelse(`9` == X9, 1, 0),
  Y10 = ifelse(`10` == X10, 1, 0)) %>% select(Y1:Y10) %>% summarise_all(funs(sum(.)/nrow(data)))

library(reshape2)

data %>% melt(id = "id") %>% group_by(variable, value) %>% summarise(percent = n()/nrow(data))
rownames(sim_data) <- 1:10
%>%melt(id = "id") %>% group_by(variable, value) %>% summarise(percent = n()/nrow(data))


uva_color_palette <- 
  c("#232D4B", #space cadet
    "#2C4F6B", #indigo dye
    "#0E879C", #blue munsell
    "#60999A", #cadet blue
    "#D1E0BF", #tea green
    "#D9E12B", #pear
    "#E6CE3A", #citrine
    "#E6A01D", #marigold
    "#E57200" #princeton orange
  )

library(TraMineR)

sts <- seqdef(data[,-11], left="DEL", gaps="DEL", right="DEL")
sts_sim <- seqdef(sim_data[,-11], left="DEL", gaps="DEL", right="DEL")

seqIplot(sts, cpal = c(uva_color_palette[5], uva_color_palette[1], 
                       uva_color_palette[2], uva_color_palette[4], 
                       uva_color_palette[6], uva_color_palette[9]),
                       sortv = "from.end", main = "True data")

seqIplot(sts_sim, cpal = c(uva_color_palette[5], uva_color_palette[1], 
                       uva_color_palette[2], uva_color_palette[4], 
                       uva_color_palette[6], uva_color_palette[9]),
         sortv = "from.end", main = "Simulated data")

