
library(ISLR)
library(gbm)
summary(Hitters)

hitters_dataset = na.omit(Hitters)

hitters_dataset$Salary = log(hitters_dataset$Salary)

set.seed(1)
train = sample(1:nrow(hitters_dataset),200)

lambda = seq(0.1,0.9,0.1)

boost.model = gbm(Salary~.,data=hitters_dataset[train,],n.trees=1000,shrinkage=0.2,distribution="gaussian",
                  interaction.depth=4)

yhat = predict(boost.model,hitters_dataset[train,],n.trees=1000)
mean((yhat-hitters_dataset$Salary[train])^2)

mse = c()
for(i in 1:length(lambda)){
    boost.model = gbm(Salary~.,data=hitters_dataset[train,],n.trees=1000,shrinkage=lambda[i],distribution="gaussian",
                      interaction.depth=4)
    yhat = predict(boost.model,newdata=hitters_dataset[train,],n.trees=1000)
    mse[i]=mean((yhat-hitters_dataset$Salary[train])^2)
}

plot(lambda,mse,type="b",ylab="training mse")

mse = c()
for(i in 1:length(lambda)){
    boost.model = gbm(Salary~.,data=hitters_dataset[train,],n.trees=1000,shrinkage=lambda[i],distribution="gaussian",
                      interaction.depth=4)
    yhat = predict(boost.model,newdata=hitters_dataset[-train,],n.trees=1000)
    mse[i]=mean((yhat-hitters_dataset$Salary[-train])^2)
}

plot(lambda,mse,ylab="test mse",type="b")

#Learning rate that gives the lowest test mse
lambda[which.min(mse)]

#lowest test mse
min(mse)

#The test mse of Linear Regression Model
lm.model = lm(Salary~.,data=hitters_dataset,subset=train)
yhat = predict(lm.model,newdata=hitters_dataset[-train,])
mean((yhat-hitters_dataset$Salary[-train])^2)

#Linear Regression with best subset selection
library(leaps)
bs.model = regsubsets(Salary~.,data=hitters_dataset,nvmax=19)
bs.model.summary = summary(bs.model)

names(bs.model.summary)

par(mfrow=c(2,2))
plot(bs.model.summary$adjr2,type="l")
plot(bs.model.summary$cp,type="l")
plot(bs.model.summary$bic,type="l")

#no. of predictors that give the highest adjusted R-squared value
which.max(bs.model.summary$adjr2)

#no. of predictors that give the lowest cp value
which.min(bs.model.summary$cp)

#no. of predictors that give the lowest bic
which.min(bs.model.summary$bic)

#mse for model with 13 variables
n = names(coef(bs.model,13))
test.mat = model.matrix(Salary~.,data=hitters_dataset[-train,])
yhat = test.mat[,n]%*%coef(bs.model,13)
mean((yhat-hitters_dataset$Salary[-train])^2)

#mse for model with 9 variables
n = names(coef(bs.model,9))
test.mat = model.matrix(Salary~.,data=hitters_dataset[-train,])
yhat = test.mat[,n]%*%coef(bs.model,9)
mean((yhat-hitters_dataset$Salary[-train])^2)

#mse for model with 3 variables
n = names(coef(bs.model,3))
test.mat = model.matrix(Salary~.,data=hitters_dataset[-train,])
yhat = test.mat[,n]%*%coef(bs.model,3)
mean((yhat-hitters_dataset$Salary[-train])^2)

summary(boost.model)

library(randomForest)

p = ncol(hitters_dataset)-1
bag.model = randomForest(Salary~.,data=hitters_dataset[train,],n.trees=500,mtry=p)

#mse of bagging algorithm
yhat = predict(bag.model,newdata=hitters_dataset[-train,])
mean((yhat-hitters_dataset$Salary[-train])^2)
