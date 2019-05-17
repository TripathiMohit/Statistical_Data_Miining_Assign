#install.packages("kohonen")
library(kohonen)

rm(list = ls())



dat <- read.csv(file = "wdbc.data.txt", sep=",", header=FALSE)

sum(is.na(dat)) ### no na values

str(dat)
dim(dat)
dat1 <- dat[,-c(1,2)]


scale_data <- scale(dat1)


###### fit SOM
set.seed(123)
som_grid <- somgrid(xdim = 6, ydim = 6, topo = "hexagonal")
df_scale_som <- som(scale_data, grid = som_grid, rlen = 10000)

quartz()
plot(df_scale_som, main="Breast Cancer Data")

codes <- df_scale_som$codes[[1]]



df_scale_som$unit.classif

df_scale_som$changes

quartz()
plot(df_scale_som,type = "changes", main="Breast Cancer Data")

quartz()
plot(df_scale_som,type = "count", main="Breast Cancer Data")

quartz()
plot(df_scale_som,type = "mapping", main="Breast Cancer Data")

coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}

quartz()
plot(df_scale_som, type = "dist.neighbours", palette.name = coolBlueHotRed)


d <- dist(codes)
hc <- hclust(d)

quartz()
plot(hc)

som_cluster_2 <- cutree(hc, k = 2)
som_cluster_2

## plot SOM with the found cluster k = 2
my_pal <- c("red", "blue", "yellow")
my_bhcol <- my_pal[som_cluster_2]
quartz()
plot(df_scale_som, type = "mapping", col = "black", bgcol = my_bhcol)
add.cluster.boundaries(df_scale_som, som_cluster_2)





## plot SOM with the found cluster k = 4
som_cluster_4 <- cutree(hc, k = 4)
som_cluster_4
my_bhcol_4 <- my_pal[som_cluster_4]
quartz()
plot(df_scale_som, type = "mapping", col = "black", bgcol = my_bhcol_4)
add.cluster.boundaries(df_scale_som, som_cluster_4)






