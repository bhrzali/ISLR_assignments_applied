
library(ISLR)

set.seed(1)
train = sample(1:nrow(Hitters),nrow(Hitters)/2)
hitters_dataset = na.omit(Hitters)
summary(hitters_dataset)

hitters_dataset$Salary = log(hitters_dataset$Salary)

set.seed(1)
library(gbm)
boost.model = gbm(Salary~.,data=hitters_dataset[train,],shrinkage=0.2,n.trees=1000,distribution="gaussian")
yhat = predict(boost.model,newdata=hitters_dataset[-train,],n.trees=1000)
#mse
mean((yhat-hitters_dataset$Salary[-train])^2)

set.seed(1)
p = ncol(hitters_dataset)-1
library(randomForest)
bag.model = randomForest(Salary~.,data=hitters_dataset,mtry=p,n.trees=1000,subset=train)
yhat = predict(bag.model,newdata=hitters_dataset[-train,])
#mse
mean((yhat-hitters_dataset$Salary[-train])^2)

set.seed(1)
p = sqrt(ncol(hitters_dataset)-1)
rf.model = randomForest(Salary~.,data=hitters_dataset,mtry=p,n.trees=1000,subset=train)
yhat=predict(rf.model,newdata=hitters_dataset[-train,])
#mse
mean((yhat-hitters_dataset$Salary[-train])^2)

lm.model = lm(Salary~.,data=hitters_dataset,subset=train)
yhat = predict(lm.model,newdata=hitters_dataset[-train,])
#mse
mean((yhat-hitters_dataset$Salary)^2)
