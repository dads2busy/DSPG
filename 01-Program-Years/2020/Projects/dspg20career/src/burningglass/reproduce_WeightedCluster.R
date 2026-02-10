library(tidyverse)
library(TraMineR)
library(WeightedCluster)

data(mvad)

mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school",
                   "training")
mvad.labels <- c("Employment", "Further Education", "Higher Education",
                    "Joblessness", "School", "Training")
mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")
subm.custom <- matrix(
  c(0, 1, 1, 2, 1, 1,
    1, 0, 1, 2, 1, 2,
    1, 1, 0, 3, 1, 2,
    2, 2, 3, 0, 3, 1,
    1, 1, 1, 3, 0, 2,
    1, 2, 2, 1, 2, 0),
  nrow = 6, ncol = 6, byrow = TRUE)

ac <- wcAggregateCases(mvad[, 17:86], weights = mvad$weight)
ac

uniqueSeq <- seqdef(mvad[ac$aggIndex, 17:86], alphabet = mvad.alphabet,
                    states = mvad.scodes, labels = mvad.labels, weights = ac$aggWeights)

mvaddist2 <- seqdist(uniqueSeq, method = "OM", indel = 1.5, sm = subm.custom)
pamclust4ac <- wcKMedoids(mvaddist2, k = 4, weights = ac$aggWeights)

mvad$acpam4 <- pamclust4ac$clustering[ac$disaggIndex]

#table(mvad$pam4, mvad$acpam4)
distinct(mvad[ac$aggIndex, 17:86]) %>% count()

head(mvad[ac$disaggIndex, 17:86], 30)

disagg <- (ac[["disaggIndex"]])

disagg <- as.data.frame(disagg)

disagg %>% rowid_to_column() %>% filter(disagg == 9)

ids <- c(79, 547, 635, 643)

mvad %>% filter(id %in% ids)

agg_small <- wcAggregateCases(small)

unique_small <- small[agg_small$aggIndex,]

small.seq <- seqdef(unique_small, weights = agg_small$aggWeights)
disagg <- (agg_small[["disaggIndex"]])
disagg <- as.data.frame(disagg)
disagg %>% rowid_to_column() %>% filter(disagg == 120)
ids <- c(342, 400, 804)
small %>% slice(342, 400, 804)


diss <- seqdist(small, method = "OM", indel = 1, sm = "TRATE", with.missing = TRUE)
clusterward <- agnes(diss, diss = TRUE, method = "ward")

agg_small <- wcAggregateCases(small)
unique_small <- small[agg_small$aggIndex,]
small.seq <- seqdef(unique_small, weights = agg_small$aggWeights)
diss <- seqdist(small.seq, method = "OM", indel = 1, sm = "TRATE", with.missing = TRUE)
clusterward_agg <- agnes(diss, diss = TRUE, method = "ward")

clusterward <- cutree(clusterward, k = 4)
clusterward_agg <- cutree(clusterward_agg, k = 4)
clusterward_fac <- factor(clusterward, labels = paste("Type", 1:4))
clusterward_agg_fac <- factor(clusterward_agg[agg_small$disaggIndex], labels = paste("Type", 1:4))

small$aggward <- clusterward_agg$height[agg_small$disaggIndex]
small$regward <- clusterward$height

table(small$aggward, small$regward)

#--------------------------------------------------------------------------------------
data(mvad)
mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school",
                      "training")
mvad.labels <- c("Employment", "Further Education", "Higher Education",
                    "Joblessness", "School", "Training")
mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")
mvadseq <- seqdef(mvad[, 17:86], alphabet = mvad.alphabet,
                     states = mvad.scodes, labels = mvad.labels,
                     weights = mvad$weight, xtstep = 6)
## Defining the custom cost matrix
subm.custom <- matrix(
    c(0, 1, 1, 2, 1, 1,
      1, 0, 1, 2, 1, 2,
      1, 1, 0, 3, 1, 2,
      2, 2, 3, 0, 3, 1,
      1, 1, 1, 3, 0, 2,
      1, 2, 2, 1, 2, 0),
    nrow = 6, ncol = 6, byrow = TRUE)
## Computing the OM dissimilarities
mvaddist <- seqdist(mvadseq, method = "OM", indel = 1.5, sm = subm.custom)

pamclust4 <- wcKMedoids(mvaddist, k=4, weights=mvad$weight)

seqdplot(mvadseq, group=pamclust4$clustering, border=NA)

print(mvadseq[unique(pamclust4$clustering), ], format="SPS")

mvad$pam4 <- factor(pamclust4$clustering, levels=c(66, 467, 607, 641), labels=c("1", "2", "3", "4"))
mvad$pam4.auto <- seqclustname(mvadseq, pamclust4$clustering, mvaddist)

ac <- wcAggregateCases(mvad[, 17:86], weights = mvad$weight)
ac

uniqueSeq <- seqdef(mvad[ac$aggIndex, 17:86], alphabet = mvad.alphabet,
                    states = mvad.scodes, labels = mvad.labels, weights = ac$aggWeights)

mvaddist2 <- seqdist(uniqueSeq, method = "OM", indel = 1.5, sm = subm.custom)
pamclust4ac <- wcKMedoids(mvaddist2, k = 4, weights = ac$aggWeights)

mvad$acpam4 <- pamclust4ac$clustering[ac$disaggIndex]

table(mvad$pam4, mvad$acpam4)

#--------------------------------------------------------------------------------------
# WeighteCluster steps

# 1. Use wcAggregate cases to create an agg object
agg_small <- wcAggregateCases(small)
# 2. Subset the sequence object by the aggIndex of the agg object to create a unique object
unique_small <- small[agg_small$aggIndex,]
# 3. Define a sequence object using the unique object with weights = aggWeights of the agg object
small.seq_w <- seqdef(unique_small, weights = agg_small$aggWeights)
# 4. Calculate distance of the sequence object
dist <- seqdist(small.seq_w, method = "OM", indel = 1, sm = "TRATE", with.missing = TRUE)
# 5. Cluster using the distance object
pamclust4_w <- wcKMedoids(dist, k=4)
# 6. Disaggregate the cluster solution by subsetting using the disaggIndex of the agg object
small$acpam4 <- pamclust4_w$clustering[agg_small$disaggIndex]

#nonweighted
small.seq_nw <- seqdef(small)
dist <- seqdist(small.seq_nw, method = "OM", indel = 1, sm = "TRATE", with.missing = TRUE)

pamclust4_nw <- wcKMedoids(dist, k=4)
#seqdplot(mvadseq, group=pamclust4$clustering, border=NA)

#print(small.seq_nw[unique(pamclust4_nw$clustering), ], format="SPS")

small$pam4 <- factor(pamclust4_nw$clustering, levels=c(918, 15, 285, 240), labels=c("1", "2", "3", "4"))

table(small$pam4, small$acpam4)

unique(pamclust4_nw$clustering)

## Using wards clustering -------------------------------------
# 1. Use wcAggregate cases to create an agg object
agg_small <- wcAggregateCases(small)
agg_small
# 2. Subset the sequence object by the aggIndex of the agg object to create a unique object
unique_small <- small[agg_small$aggIndex,]
# 3. Define a sequence object using the unique object with weights = aggWeights of the agg object
small.seq_w <- seqdef(unique_small, weights = agg_small$aggWeights)
# 4. Calculate distance of the sequence object
dist <- seqdist(small.seq_w, method = "HAM")
# 5. Cluster using the distance object
clusterward_w <- hclust(as.dist(dist), method = "average", members = agg_small$aggWeights)
clusterward_w_8 <- cutree(clusterward_w, k = 4)

#small.seq_nw <- seqdef(small)
dist <- seqdist(small, method = "HAM")

clusterward_nw <- hclust(as.dist(dist), method = "average")
clusterward_nw_8 <- cutree(clusterward_nw, k = 4)

# 6. Disaggregate the cluster solution by subsetting using the disaggIndex of the agg object
small$w <- clusterward_w_8[agg_small$disaggIndex]
small$nw <- clusterward_nw_8

#unique(pamclust4_nw$clustering)

#small$nw <- factor(pamclust4_nw$clustering, levels=c(918, 15, 285, 240), labels=c("1", "2", "3", "4"))

table(small$w, small$nw)

clusterward_nw_8_fac <- factor(clusterward_nw_8, labels = paste("Type", 1:8))
clusterward_w_8_fac <- factor(clusterward_w_8[agg_small$disaggIndex], labels = paste("Type", 1:8))

n_plot <- seqIplot(small[,1:30], group = clusterward_n_8_fac, border = NA, use.layout = TRUE, cols = 8, withlegend = F, sortv = "from.start", main = "regular")
w_plot <- seqIplot(small30], group = clusterward_w_8_fac, border = NA, use.layout = TRUE, cols = 8, withlegend = F, sortv = "from.start", main = "weighted")
seqlegend(large)

clusterward_w_8[agg_small$disaggIndex]
clusterward_nw_8
