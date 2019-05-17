install.packages("cluster")

library(cluster)


rm(list = ls())


##part a.)
dat <- read.csv(file = "Ch10Ex11.csv", header=FALSE)


## part b.)
######
# correlation based distance
corr <- cor(dat)
dis_cor <- as.dist(1 - corr)


#####Hierchical clustering single linkage
hc_sing <- hclust(dis_cor,method="single")
quartz()
plot(hc_sing) ## gives 2 clusters




#####Hierchical clustering complete linkage
hc_comp <- hclust(dis_cor,method="complete")
quartz()
plot(hc_comp) ### gives 2 clusters


#####Hierchical clustering average linkage
hc_ave <- hclust(dis_cor,method="average")
quartz()
plot(hc_ave) ### gives 3 clusters

#####Hierchical clustering centroid linkage
hc_cent <- hclust(dis_cor,method="centroid")
quartz()
plot(hc_cent)


###part c.)

pr_out=prcomp(t(dat))
head(order(rowSums(abs(pr_out$rotation)),decreasing = TRUE))




