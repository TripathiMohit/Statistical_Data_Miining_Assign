install.packages('mclust')
install.packages('ggfortify')


library(mclust)
library(ggfortify)


rm(list=ls())
data(banknote)

dat= banknote


dat = dat[,2:7]

##### full dataset
pca_full=prcomp(dat, scale=TRUE)

summary(pca_full)

names(pca_full)
pca_full$center
pca_full$scale
pca_full$rotation

quartz()
autoplot(pca_full, shape=2 , colour=dat$Status, loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3)
autoplot(pca_full,x=2,y=3, shape=2 , colour=dat$Status, loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3)

pca_variance = pca_full$sdev^2
pve = pca_variance/sum(pca_variance)  #### explained variance
plot(pve,xlab="Principal Component", ylab="Proportion of explained variance ",main='PVE for full data set', ylim=c(0,1),type='b')
plot(cumsum(pve),xlab="Principal Component", ylab="Proportion of cumulative sum of explained variance ",main='Cummulative Variance for full dataset', ylim=c(0,1),type='b')
pve
cumsum(pve)


## PCR for Genuine Notes

dat_gen= banknote[banknote$Status=='genuine',2:7]
pca_gen=prcomp(dat_gen, scale=TRUE)

quartz()
autoplot(pca_gen, shape=2 , loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3,main='PCA for Genuine Notes, PC1 and PC2')
autoplot(pca_gen,x=2,y=3, shape=2 , loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3,main='PCA for Genuine Notes, PC2 and PC3')
pca_variance_gen = pca_gen$sdev^2
pve_gen = pca_variance_gen/sum(pca_variance_gen)  #### explained variance
plot(pve_gen,xlab="Principal Component", ylab="Proportion of explained variance", main='PVE for Genuine Notes', ylim=c(0,1),type='b')
plot(cumsum(pve_gen),xlab="Principal Component", ylab="Proportion of cumulative sum of explained variance  ",main='Cummulative Variance for Genuine Notes', ylim=c(0,1),type='b')


##  PCR for Counterfeit Notes


dat_count= banknote[banknote$Status != 'genuine',2:7]
pca_count=prcomp(dat_count, scale=TRUE)

quartz()
autoplot(pca_count, shape=2 , loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3,main='PCA for counterfeit Notes, PC1 and PC2')
autoplot(pca_count,x=2,y=3, shape=2 , loadings = TRUE, loadings.colour='Orange', loadings.label = TRUE, loadings.label.size = 3,main='PCA for counterfeit Notes, PC2 and PC3')
pca_variance_count = pca_count$sdev^2
pve_count = pca_variance_count/sum(pca_variance_count)
plot(pve_count,xlab="Principal Component", ylab="Proportion of explained variance", main='PVE for Counterfeit Notes', ylim=c(0,1),type='b')
plot(cumsum(pve_count),xlab="Principal Component", ylab="Proportion of cumulative sum of explained variance ",main='Cummulative Variance for Counterfeit Notes', ylim=c(0,1),type='b')
