
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

## plot errors
plot(cv_error,main='MSE vs hidden neurons',xlab="Hidden neurons", ylab='MSE',type='l',col='red',lwd=2)

min_err_neuron = min(which(min(cv_error) == cv_error))
min_err_neuron
net <- neuralnet(form, data=train, hidden=c(min_err_neuron), act.fct = "logistic",err.fct = 'sse', linear.output=FALSE, threshold=0.15)
predict_net <- compute(net, test[,1:57])

## test error
test_error <- mean(round(predict_net$net.result) != test$spam)
test_error   ### 6.69%



##Logistic regression model
glm1 <- glm(spam ~ ., data = train,family = "binomial")
summary(glm1)

fitglm_train <- predict(glm1, newdata = train, type = "response")
y_hat_train <- round(fitglm_train)

fitglm_test <- predict(glm1, newdata = test, type = "response")
y_hat_test <- round(fitglm_test)

train_err <- sum(abs(y_hat_train - train$spam))/length(train$spam)
train_err ### train_error with logistic regression is 7.30 %
test_err <- sum(abs(y_hat_test - test$spam))/length(test$spam)
test_err  ### test_error with logistic regression is 6.43 %


