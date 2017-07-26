
library(ISLR)
OJ = na.omit(OJ)
summary(OJ)

set.seed(1)
train = sample(nrow(OJ),800)

library(e1071)
set.seed(1)
svm.model = svm(Purchase~.,data=OJ[train,],kernel="linear",cost=0.01)
summary(svm.model)

#Training error
pred.train = predict(svm.model,newdata=OJ[train,])
mean(pred.train!=OJ[train,]$Purchase)*100

#Test error
pred.test = predict(svm.model,newdata=OJ[-train,])
mean(pred.test!=OJ[-train,]$Purchase)*100

set.seed(1)
range = list(cost=c(0.01,0.1,1,5,10))
tune.linear = tune(svm,Purchase~.,data=OJ[train,],kernel="linear",ranges=range)
summary(tune.linear)

#Training error with cost = 0.01
pred.train = predict(tune.linear$best.model,newdata=OJ[train,])
mean(pred.train!=OJ[train,]$Purchase)*100
table(pred.train,OJ[train,]$Purchase)

#Test error with cost = 0.01
pred.test = predict(tune.linear$best.model,newdata=OJ[-train,])
mean(pred.test!=OJ[-train,]$Purchase)*100
table(pred.test,OJ[-train,]$Purchase)

set.seed(2)
svm.radial = svm(Purchase~.,data=OJ[train,],kernel="radial",cost=0.01)
summary(svm.radial)

#Training error
pred.radial = predict(svm.radial,newdata=OJ[train,])
mean(pred.radial!=OJ[train,]$Purchase)*100

#Test error
pred.radial = predict(svm.radial,newdata=OJ[-train,])
mean(pred.radial!=OJ[-train,]$Purchase)*100

#cross validation
set.seed(1)
range = list(cost=c(0.01,0.1,1,5,10))
tune.radial = tune(svm,Purchase~.,data=OJ[train,],kernel="radial",ranges=range)
summary(tune.radial)

#Training error
pred.train = predict(tune.radial$best.model,newdata=OJ[train,])
mean(pred.train!=OJ[train,]$Purchase)*100
table(pred.train,OJ[train,]$Purchase)

#Test error
pred.test = predict(tune.radial$best.model,newdata=OJ[-train,])
mean(pred.test!=OJ[-train,]$Purchase)*100
table(pred.test,OJ[-train,]$Purchase)

set.seed(1)
svm.poly = svm(Purchase~.,data=OJ[train,],kernel="polynomial",degree=2,cost=0.01)
summary(svm.poly)

#Training error
pred.poly = predict(svm.poly,newdata=OJ[train,])
mean(pred.poly!=OJ[train,]$Purchase)*100
table(pred.poly,OJ[train,]$Purchase)

#Test error
pred.poly = predict(svm.poly,newdata=OJ[-train,])
mean(pred.poly!=OJ[-train,]$Purchase)*100
table(pred.poly,OJ[-train,]$Purchase)

#cross validation
range = list(cost=c(0.01,0.1,1,5,10))
tune.poly = tune(svm,Purchase~.,data=OJ[train,],kernel="polynomial",degree=2,ranges=range)
summary(tune.poly)

#Training error
pred.train = predict(tune.poly$best.model,newdata=OJ[train,])
mean(pred.train!=OJ[train,]$Purchase)*100
table(pred.train,OJ[train,]$Purchase)

#Test error
pred.test = predict(tune.poly$best.model,newdata=OJ[-train,])
mean(pred.test!=OJ[-train,]$Purchase)*100
table(pred.test,OJ[-train,]$Purchase)
