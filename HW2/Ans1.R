install.packages("cluster")

library(cluster)


rm(list = ls())
set.seed(123)
dat <- USArrests

# part a:)
#####
d <- dist(dat)
hc <- hclust(d, method = "complete")

quartz()
plot(hc, hang = -1)


# part b:)
#####
ct <-  cutree(hc, h = 150)
ct
si  <- silhouette(ct, dist = d)
plot(si)

#part c:)
####
scale_data <- scale(dat)
#?sd

d1 <- dist(scale_data)
hc1 <- hclust(d1, method = "complete")

quartz()
plot(hc1, hang = -1)

#part d:)
ct1 <- cutree(hc1, k=3)
ct1
si1  <- silhouette(ct1, dist = d1)
plot(si1)