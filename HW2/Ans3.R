install.packages("caret")
install.packages("fpc")


library(caret)
library(fpc)



rm(list = ls())

load("/Users/mohittripathi/SEM2/primate.scapulae.rda")
dat <- primate.scapulae

#### since the column gamma has NA values for a class and there's no 
#### description about this column, I have removed this column from the dataset

dat <- primate.scapulae[,-9]
dats <- dat[ ,1:8]

### scaling the data
dat_scale <- scale(dats)
d <- dist(dat_scale)
dim(as.matrix(d))


##### average linkage
hc_ave <- hclust(d, method = "ave")

quartz()
plot(hc_ave, hang = -1)

ct <- cutree(hc_ave, k = 5)
si <- silhouette(ct, dist = d)
quartz()
plot(si) ### average width .41
table(ct, dat$classdigit)


confusionMatrix(as.factor(ct),dat$classdigit)



#### complete linkage
hc_comp <- hclust(d, method = "complete")
quartz()
plot(hc_comp, hang = -1)
ct_comp <- cutree(hc_comp, k = 5)
si_comp <- silhouette(ct_comp, dist = d)
quartz()
plot(si_comp) ### average width .41
table(ct_comp, dat$classdigit)


confusionMatrix(as.factor(ct_comp),dat$classdigit)



### single linkage
hc_sin <- hclust(d, method = "single")
quartz()
plot(hc_sin, hang = -1)
ct_sin <- cutree(hc_sin, k = 5)
si_sin <- silhouette(ct_sin, dist = d)
quartz()
plot(si_sin) ### average width .31
table(ct_sin, dat$classdigit)


confusionMatrix(as.factor(ct_sin),dat$classdigit)




##### part b.)
## k-mediods
##############################
kmed <- pamk(dat_scale)

# let the program decide optimal k
kmed$nc ### 4 clusters

# tabulate the results as before
table(kmed$pamobject$clustering, dat$classdigit)

# plot the results for k= 4
layout(matrix(c(1,2), 1, 2))
plot(kmed$pamobject)


## use k mediods for k =5 to find true misclassification rate
kmed_5 <- pam(dat_scale, k =5)
# tabulate the results as before
table(kmed_5$clustering, dat$classdigit)

confusionMatrix(as.factor(kmed_5$clustering),dat$classdigit) 
## 94% accuracy






