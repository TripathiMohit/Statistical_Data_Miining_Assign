install.packages("ggplot2")
install.packages("ElemStatLearn")
install.packages("randomForest")
library(ggplot2)



library(ElemStatLearn)




library(randomForest)

rm(list=ls())
dat<-spam
set.seed(123)
train_dat=sample(1:nrow(dat),0.80*nrow(dat))
train<-dat[train_dat,]
test <- dat[-train_dat,]
model = c()
test_error_list = c()

for(i in seq(1,15,2)){
  rf_fit = randomForest(spam~.,data = train, ntree =100, mtry = i)
  
  pred_test <- predict(rf_fit, newdata = test, type='class')

  test_err <- mean(pred_test != test$spam)

  model = c(model,i)
  test_error_list = c(test_error_list,test_err)
}

error = data.frame(Model_Name = model,Test_Error = test_error_list)

plot(error$Model_Name,error$Test_Error,type='o', xlab="M values" , ylab="Test Error")


collection = rep(0, 100)
for(i in seq(1,15,2)){
  rf_fit = randomForest(spam~., data  = train, ntree = 100, mtry = i)
  predictions = predict(rf_fit, test, type = "class")
  collection = cbind(collection, rf_fit$err.rate[,c(1)])
}



collection_plot = data.frame(collection)[,-c(1)]
names(collection_plot) = c("mtry_1", "mtry_3","mtry_5","mtry_7","mtry_9","mtry_11","mtry_13","mtry_15")
collection_plot$NTrees = seq(1, 100)





ggplot(collection_plot, aes(NTrees)) + 
  geom_line(aes(y = mtry_1, colour = "mtry_1")) + geom_point(aes(y = mtry_1), size = 0.3) + 
  geom_line(aes(y = mtry_3, colour = "mtry_3")) + geom_point(aes(y = mtry_3), size = 0.3) + 
  geom_line(aes(y = mtry_5, colour = "mtry_5")) + geom_point(aes(y = mtry_5), size = 0.3) + 
  geom_line(aes(y = mtry_7, colour = "mtry_7")) + geom_point(aes(y = mtry_7), size = 0.3) + 
  geom_line(aes(y = mtry_9, colour = "mtry_9")) + geom_point(aes(y = mtry_9), size = 0.3) + 
 
  
  ggtitle("Out of Bag Errors.") + xlab("Trees") + ylab("OOB Error") +
  theme(plot.title = element_text(hjust = 0.5))


