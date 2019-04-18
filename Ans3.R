install.packages("arules")
install.packages("ElemStatLearn")
install.packages("MASS")
install.packages("plyr")


library("arules")
library("ElemStatLearn")
library("MASS")
library("plyr")

rm(list = ls())
data(Boston)
head(Boston)

dat <- Boston



hist(dat$crim,ylim = c(0,500), main="Histogram for per capita crime rate", xlab = "crime rate", border="black", col="blue") 
summary(dat$crim)
dat[["crim"]] <- ordered(cut(dat[["crim"]], c(0,10,30,90), labels = c("low rate", "medium rate", "high rate") ) )
dat$crim
summary(dat$crim)


hist(dat$zn, ylim = c(0,400), main="Residential land zoned for lots over 25,000 sq.ft.", xlab = "Proportion of residential land", border="black", col="blue")
summary(dat$zn)
dat[["zn"]] <- ordered(cut(dat[["zn"]], c(-1,25,50,100), labels = c("small land", "medium land", "large land") ) )
dat$zn
summary(dat$zn)


hist(dat$indus,xlim = c(0,30), ylim = c(0,200), main="Non-retail business acres per town", xlab = "proportion of non-retail business acres", border="black", col="blue")
summary(dat$indus)
dat[["indus"]] <- ordered(cut(dat[["indus"]], c(0,8,18,28), labels = c("low business", "medium business", "high business") ) )
dat$indus
summary(dat$indus)


hist(dat$chas,  ylim = c(0,500), main="Charles River dummy variable", xlab = "Variable Value", border="black", col="blue")

#dat$chas <- as.factor(dat$chas)
dat[["chas"]] <- ordered(cut(dat[["chas"]], c(-1,0.6,1), labels = c("No River", "River" ) ))


#dat$chas[dat$chas == 0] <- "No"
#dat$chas[dat$chas == 1] <- "Yes"
#dat$chas
summary(dat$chas)



hist(dat$nox, ylim = c(0,140), main="Nitric oxides concentration", xlab = "Parts per 10 million", border="black", col="blue")
summary(dat$nox)
dat[["nox"]] <- ordered(cut(dat[["nox"]], c(0.36,0.56,.68,1), labels = c("low NOX", "medium NOX", "high NOX") ) )
dat$nox
summary(dat$nox)


hist(dat$rm, ylim = c(0,200), main="Average number of rooms per dwelling", xlab = "Rooms", border="black", col="blue")
summary(dat$rm)
dat[["rm"]] <- ordered(cut(dat[["rm"]], c(3.5,5.9,6.9,9), labels = c("less rooms", "medium rooms", "high rooms") ) )
dat$rm
summary(dat$rm)


hist(dat$age, ylim = c(0,200), main="Proportion of owner-occupied units", xlab = "Units", border="black", col="blue")
summary(dat$age)
dat[["age"]] <- ordered(cut(dat[["age"]], c(0,35,70,100), labels = c("low units", "medium units", "high units") ) )
dat$age
summary(dat$age)


hist(dat$dis, xlim = c(1,14), ylim = c(0,150), main="Distances to five Boston employment centres", xlab = "Distance", border="black", col="blue")
summary(dat$dis)
dat[["dis"]] <- ordered(cut(dat[["dis"]], c(1,3,5.5,13), labels = c("less distance", "medium distance", "high distance") ) )
dat$dis
summary(dat$dis)


hist(dat$rad, xlim = c(0,25), ylim = c(0,160), main="Index of accessibility to radial highways", xlab = "Index", border="black", col="blue")
summary(dat$rad)
dat[["rad"]] <- ordered(cut(dat[["rad"]], c(0,4.5,9,25), labels = c("low index", "medium index", "high index") ) )
dat$rad
summary(dat$rad)



hist(dat$tax, xlim = c(160,780), ylim = c(0,160), main="Full-value property-tax rate per $10,000", xlab = "Rate", border="black", col="blue")
summary(dat$tax)
dat[["tax"]] <- ordered(cut(dat[["tax"]], c(180,300,500,750), labels = c("low tax", "medium tax", "high tax") ) )
dat$tax
summary(dat$tax)


hist(dat$ptratio, xlim = c(12,23), ylim = c(0,210), main="Pupil-teacher ratio by town", xlab = "Ratio", border="black", col="blue")
summary(dat$ptratio)
dat[["ptratio"]] <- ordered(cut(dat[["ptratio"]], c(12,16,20,23), labels = c("low ptratio", "medium ptratio", "high ptratio") ) )
dat$ptratio
summary(dat$ptratio)


hist(dat$black, xlim = c(0,400), ylim = c(0,500), main="Proportion of blacks by town", xlab = "Blacks", border="black", col="blue")
summary(dat$black)
dat[["black"]] <- ordered(cut(dat[["black"]], c(0,100,350,400), labels = c("low Bk", "medium Bk", "high Bk") ) )
dat$black
summary(dat$black)


hist(dat$lstat, xlim = c(1,40), ylim = c(0,500), main="% lower status of the population", xlab = "Percentage", border="black", col="blue")
summary(dat$lstat)
dat[["lstat"]] <- ordered(cut(dat[["lstat"]], c(0,10,17,40), labels = c("low lstat", "medium lstat", "high lstat") ) )
dat$lstat
summary(dat$lstat)

hist(dat$medv, xlim = c(2,55), ylim = c(0,200), main="Median value of owner-occupied homes", xlab = "Value in $1000's", border="black", col="blue")
summary(dat$medv)
dat[["medv"]] <- ordered(cut(dat[["medv"]], c(4,17,25,51), labels = c("low MEDV", "medium MEDV", "high MEDV") ) )
dat$medv
summary(dat$medv)


## convert to a binary incidence matrix

Data_binary <- as(dat, "transactions")
summary(Data_binary)


#####Part b:)
quartz()
itemFrequencyPlot(Data_binary , support=0.01, cex.names= .8)

## apply the apriori
rules <- apriori(Data_binary, parameter = list( support=.01, confidence=.6 , maxlen = 15))
summary(rules)


####### part c:)
ruleCityLowDis <- subset(rules, subset= rhs %in% "dis=less distance" & lift>1.2)
ruleLowCrime <- subset(rules, subset = rhs %in% "crim=low rate" & lift >.6)
summary(ruleCityLowDis)

summary(ruleLowCrime)

rules_sub <- subset(rules, subset = rhs %in% "crim=low rate" & lhs %in% "dis=less distance" & lift >.5)
summary(rules_sub)
inspect(head(sort(rules_sub, by ='lift'),n = 5))
inspect(head(sort(rules_sub, by ='confidence'),n = 5))





#####part d:)
rulesLowPtratio <- subset(rules, subset = rhs %in% "ptratio=low ptratio" & lift >.5)
summary(rulesLowPtratio)
inspect(head(sort(rulesLowPtratio, by ='lift'),n = 5))
inspect(head(sort(rulesLowPtratio, by ='confidence'),n = 5))
##to stay in an area with high NOX (Nitrogen Oxides Concentration) and medium 
##RAD (index of accessibility to radial highways).

##stay in an area with high NOX (Nitrogen Oxides Concentration) and medium 
##TAX value (Full Value property tax rate per $10,000) 


#####part e:)
model = lm(Boston$ptratio~.,data=Boston)
summary(model)
### significant variables (by checking p value) includes NOX and RAD with 3 stars.
### However tax is not significant variable to predict ptratio as per lm model










