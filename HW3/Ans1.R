###################################################################
rm(list = ls())
# install.packages("Rgraphviz")
# install.packages("gRbase")
# install.packages("gRain")
# install.packages("ggm")
# install.packages("gRim")
# install.packages("bnlearn")
# 
# source("http://people.math.aau.dk/~sorenh/grinstall.R");
# grinstall()
# 
# source("http://bioconductor.org/biocLite.R"); 
# biocLite(c("graph", "RBGL", "Rgraphviz"))
# 
# install.packages("gRbase", dependencies=TRUE); 
# install.packages("gRain", dependencies=TRUE); 
# install.packages("gRim", dependencies=TRUE)

# gRinstall <- function(){
#   source("http://bioconductor.org/biocLite.R")
#   biocLite(c("graph","RBGL","Rgraphviz"))
#   
#   install.packages("gRbase", dependencies=TRUE)
#   install.packages("gRain", dependencies=TRUE)
#   install.packages("gRim", dependencies=TRUE)
# }


library(Rgraphviz)
library(gRbase)
library(gRain)
library(ggm)
library(gRim)
library(bnlearn)

###################################################################

?cad1
data(cad1)

#part a
# construct network
names(cad1)
dat = list(~sex, ~smoker|sex, ~cad|inherit:hyperchol, ~suffheartf, ~hyperchol|suffheartf:smoker, ~inherit|smoker)
?dagList
cad_graph = dagList(dat)

plot(cad_graph)

# finding links
dSep(as(cad_graph, "matrix"),"sex", "suffheartf",cond = NULL)

dSep(as(cad_graph, "matrix"),"smoker", "suffheartf",cond = NULL)

dSep(as(cad_graph, "matrix"),"inherit", "suffheartf",cond = NULL)

dSep(as(cad_graph, "matrix"),"smoker","cad",c("inherit","hyperchol"))

dSep(as(cad_graph, "matrix"),"sex", "hyperchol", c("smoker", "suffheartf"))

dSep(as(cad_graph, "matrix"), "sex", "inherit", c("smoker"))

?xtabs
fullandsex = xtabs(~Sex, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
smokerandsex = xtabs(~Smoker+Sex, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
suffheartfandobj = xtabs(~SuffHeartF, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
inheritandsmoker = xtabs(~Inherit+Smoker , data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
cadandhypercholandinherit = xtabs(~CAD+Inherit+Hyperchol, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
hypercholandsuffheartfandsmoker = xtabs(~Hyperchol+Smoker+SuffHeartF, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])

fullandsex
smokerandsex
suffheartfandobj
inheritandsmoker
cadandhypercholandinherit
hypercholandsuffheartfandsmoker


## part b
?compileCPT
ls = compileCPT(list(fullandsex, smokerandsex, suffheartfandobj, inheritandsmoker, cadandhypercholandinherit, hypercholandsuffheartfandsmoker)) 
lscomp = grain(ls)


### Compile and Propagate the network
gcomp = compile(lscomp)
gcomp = propagate(gcomp)
summary(gcomp)

# when we have the value of the evidence
gcomp.ev = setFinding(gcomp, nodes = c("Sex", "Hyperchol"), states = c("Female", "yes"))

querygrain(gcomp, nodes = c("SuffHeartF", "CAD"), type = "joint")
querygrain(gcomp.ev, nodes = c("SuffHeartF", "CAD"), type = "joint")


querygrain(gcomp, nodes = c("SuffHeartF", "CAD"), type = "conditional")
querygrain(gcomp.ev, nodes = c("SuffHeartF", "CAD"), type = "conditional")


querygrain(gcomp, nodes = c("SuffHeartF", "CAD"), type = "marginal")
querygrain(gcomp.ev, nodes = c("SuffHeartF", "CAD"), type = "marginal")


## part c

# Upon simulating 5 new observations

find5 = simulate(gcomp.ev, nsim = 5)
predict(gcomp, response = c("Smoker","CAD"),newdata = find5, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
find5

## part d

# Upon simulating 500 new observations

find500 = simulate(gcomp.ev, nsim = 500)
pval = predict(gcomp, response = c("Smoker","CAD"),newdata = find500, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")

save(find500, file="find500.RData")

# calculating the misclassification rate

# smoker

table(find500$Smoker, pval$pred$Smoker)
mean(find500$Smoker != pval$pred$Smoker)

# CAD
tab2 = table(find500$CAD, pval$pred$CAD)
mean(find500$CAD != pval$pred$CAD)