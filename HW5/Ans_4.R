install.packages("ISLR")
install.packages("e1071")
library(ISLR)
library(e1071)


rm (list=ls())



data(OJ)
attach(OJ)
set.seed(123)
sam <- sample(1:nrow(OJ), 0.30*nrow(OJ))
test <- OJ[sam,]
train <- OJ[-sam,]

test_error_lin <- c()
train_error_lin <- c()


##### Linear Kernel

for (i in c(0.01,0.1, 1, 5, 10)){
  
  svm_linear <- tune(svm, Purchase ~ .,data = train, kernel = "linear",ranges = list(cost = i))
  
  best_model <- svm_linear$best.model
  best_model
  y_hat <- predict(best_model, newdata = test)
  y_true <- test$Purchase
  
  test_error1 <- length(which(y_true != y_hat))/length(y_true)
  test_error_lin<-c(test_error_lin,test_error1)    
  
  y_hat <- predict(best_model, newdata = train)
  y_true <- train$Purchase
  
  train_error2 <- length(which(y_true != y_hat))/length(y_true)
  train_error_lin<-c(train_error_lin,train_error2)
  
}

test_error_lin
train_error_lin
which(min(test_error_lin) == test_error_lin)    ### optimal cost is 10
 

upper_lim = max(test_error_lin, train_error_lin)
lower_lim = min(test_error_lin, train_error_lin)

plot(train_error_lin, type = "o", lty = 2, col = "blue", ylim = c(lower_lim -1, upper_lim +1) , xlab = "cost", ylab = "error", main = "Train and Test Errors for linear kernal")
lines(test_error_lin, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue","red"))


####  Radial Kernel

train_error_rad <- c()
test_error_rad<- c()

for (i in c(0.01,0.1, 1, 5, 10)){
  svm_rad <- tune(svm, Purchase ~ .,data = train, kernel = "radial",ranges = list(cost = i))
  
  best_model <- svm_rad$best.model
  best_model
  
  ### predict test data ###
  
  y_hat <- predict(best_model, newdata = test)
  y_true <- test$Purchase
  
  test_error3 <- length(which(y_true != y_hat))/length(y_true)
  test_error_rad<-c(test_error_rad,test_error3)
  
  y_hat <- predict(best_model, newdata = train)
  y_true <- train$Purchase
  
  train_error2 <- length(which(y_true != y_hat))/length(y_true)
  train_error_rad<-c(train_error_rad,train_error2)
  
}

test_error_rad
train_error_rad

which(min(test_error_rad) == test_error_rad) ##### optimal cost is at cost 1



upper_lim = max(test_error_rad, train_error_rad)
lower_lim = min(test_error_rad, train_error_rad)

x11()
plot(train_error_rad, type = "o", lty = 2, col = "blue", ylim = c(lower_lim -1, upper_lim +1) , xlab = "cost", ylab = "error", main = "Train and Test Errors for Radial Kernel")
lines(test_error_rad, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue","red"))


###### Polynimal Kernel


train_error_pol <- c()
test_error_pol<- c()

for (i in c(0.01,0.1, 1, 5, 10)){
  svm_pol <- tune(svm, Purchase ~ .,data = train, degree = 2, kernel = "polynomial",ranges = list(cost = i))
  
  best_model <- svm_pol$best.model
  best_model
  
  ### predict test data ###
  
  y_hat <- predict(best_model, newdata = test)
  y_true <- test$Purchase
  
  test_error5 <- length(which(y_true != y_hat))/length(y_true)
  test_error_pol<-c(test_error_pol,test_error5)
  
  y_hat <- predict(best_model, newdata = train)
  y_true <- train$Purchase
  
  train_error6 <- length(which(y_true != y_hat))/length(y_true)
  train_error_pol<-c(train_error_pol,train_error6)
  
}

test_error_pol
train_error_pol

which(min(test_error_pol) == test_error_pol) ###optimal cost is at cost 5


upper_lim = max(test_error_pol, train_error_pol)
lower_lim = min(test_error_pol, train_error_pol)

x11()
plot(train_error_pol, type = "o", lty = 2, col = "blue", ylim = c(lower_lim -1, upper_lim +1) , xlab = "cost", ylab = "error", main = "Train and Test Errors for polynomial Kernel")
lines(test_error_pol, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue","red"))
