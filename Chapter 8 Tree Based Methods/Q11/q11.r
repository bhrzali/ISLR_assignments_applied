
library(ISLR)

caravan_dataset = Caravan
p = rep(0,nrow(caravan_dataset))
p[caravan_dataset$Purchase=="Yes"]=1
caravan_dataset$Purchase = p

set.seed(1)
train = 1:1000

library(gbm)
boost.model = gbm(Purchase~.,data=caravan_dataset[train,],shrinkage=0.01,n.trees=1000,distribution="bernoulli")

summary(boost.model)

yhat = predict(boost.model,newdata=caravan_dataset[-train,],n.trees=1000,type="response")
purchase.pred = rep("No",length(yhat))
purchase.pred[yhat>0.2]="Yes"
table(purchase.pred,Caravan$Purchase[-train])

#Fraction of people predicted to make a purchase that do in fact make one. (Fraction of True Positives)
33/(123+33)

#knn
library(class)
std.x = scale(Caravan[,-86])
train.x = std.x[train,]
train.y = Caravan[train,86]
test.x = std.x[-train,]

set.seed(1)
knn.pred = knn(train.x,test.x,train.y,k=5)
table(knn.pred,Caravan$Purchase[-train])

#Fraction of people predicted to make a purchase that do in fact make one. (Fraction of True Positives)
10/(27+10)

#Logistic Regression
glm.model = glm(Purchase~.,data=Caravan,family=binomial,subset=train)

yhat = predict(glm.model,newdata=Caravan[-train,],type="response")
glm.pred = rep("No",length(yhat))
glm.pred[yhat>=0.5] = "Yes"
table(glm.pred,Caravan$Purchase[-train])

#Fraction of people predicted to make a purchase that do in fact make one. (Fraction of True Positives)
15/(15+87)
