

install.packages("neuralnet")
install.packages("ElemStatLearn")

library(ElemStatLearn)
library(neuralnet)

rm(list=ls())

set.seed(12)
data(spam)
spam$spam <- ifelse(spam$spam == "spam",1,0)
spam<- spam[1:4600,]

name <- names(spam)
form <- as.formula(paste("spam ~", paste(name[!name %in% "spam"], collapse = " + ")))
crossvalidate <- function(spam, hidden_1)
{  
  
  sam <- sample(1:nrow(spam), round(nrow(spam)*0.75))
  train_data<- spam[sam,]
  cv_data <- spam[-sam,]
  
  net <- neuralnet(form, data=train_data, hidden=hidden_1, act.fct = "logistic",err.fct = 'sse', linear.output=FALSE,threshold=0.15)
  predict_net <- compute(net,cv_data[,1:57])
  
  predict <- round(predict_net$net.result)
  error_cv <- mean(predict != cv_data$spam)
  return(error_cv)
}

samp <- sample(1:nrow(spam), round(nrow(spam)*0.75))
train<- spam[samp,]
test <- spam[-samp,]

test_error <- NULL
cv_error <- NULL
set.seed(121)


## calling the cross validation function
for(i in 1:5)
{
  cv_error[i] = crossvalidate(spam,hidden=c(i))
}

min_err_neuron = min(which(min(cv_error) == cv_error))
min_err_neuron   #### cv error is minimum with 4 neurons
set.seed(121)
net <- neuralnet(form, data=train, hidden=c(min_err_neuron), act.fct = "logistic",err.fct = 'sse', linear.output=FALSE, threshold=0.15)
predict_net <- compute(net, test[,1:57])

## test error
test_error <- mean(round(predict_net$net.result) != test$spam)
test_error   ### test error is  5.73% with original data



#### effect of outlier when outlier has value 100

trainDataOutLier = train
trainDataOutLier[1,4] = 100 

test_error <- NULL
cv_error <- NULL
set.seed(121)
net <- neuralnet(form, data=trainDataOutLier, hidden=c(min_err_neuron), act.fct = "logistic",err.fct = 'sse', linear.output=FALSE, threshold=0.15)
predict_net <- compute(net, test[,1:57])

## test error
test_error <- mean(round(predict_net$net.result) != test$spam)
test_error   ### 5.82%


#### effect of outlier when outlier has value 25
trainDataOutLier[1,4] = 25 

test_error <- NULL
cv_error <- NULL
set.seed(1)
net <- neuralnet(form, data=trainDataOutLier, hidden=c(min_err_neuron), act.fct = "logistic",err.fct = 'sse', linear.output=FALSE, threshold=0.15)
predict_net <- compute(net, test[,1:57])

## test error
test_error <- mean(round(predict_net$net.result) != test$spam)
test_error   ### 6.17%


#### effect of outlier when outlier has value 20
trainDataOutLier[1,4] = 20 

test_error <- NULL
cv_error <- NULL
set.seed(121)
net <- neuralnet(form, data=trainDataOutLier, hidden=c(min_err_neuron), act.fct = "logistic",err.fct = 'sse', linear.output=FALSE, threshold=0.15)
predict_net <- compute(net, test[,1:57])

## test error
test_error <- mean(round(predict_net$net.result) != test$spam)
test_error   ### 6.17%


#### effect of outlier when outlier has value 2
trainDataOutLier[1,4] = 2 

test_error <- NULL
cv_error <- NULL
set.seed(121)
net <- neuralnet(form, data=trainDataOutLier, hidden=c(min_err_neuron), act.fct = "logistic",err.fct = 'sse', linear.output=FALSE, threshold=0.15)
predict_net <- compute(net, test[,1:57])

## test error
test_error <- mean(round(predict_net$net.result) != test$spam)
test_error   ### 6.08%

#### effect of outlier when outlier has value .2
trainDataOutLier[1,4] = .2 

test_error <- NULL
cv_error <- NULL
set.seed(123)
net <- neuralnet(form, data=trainDataOutLier, hidden=c(min_err_neuron), act.fct = "logistic",err.fct = 'sse', linear.output=FALSE, threshold=0.15)
predict_net <- compute(net, test[,1:57])

## test error
test_error <- mean(round(predict_net$net.result) != test$spam)
test_error   ### 6.086%




#### effect of outlier when outlier has original value
trainDataOutLier[1,4] = 0 
test_error <- NULL
cv_error <- NULL
set.seed(121)

net <- neuralnet(form, data=trainDataOutLier, hidden=c(min_err_neuron), act.fct = "logistic",err.fct = 'sse', linear.output=FALSE, threshold=0.15)
predict_net <- compute(net, test[,1:57])

## test error
test_error <- mean(round(predict_net$net.result) != test$spam)
test_error   ### 5.73%