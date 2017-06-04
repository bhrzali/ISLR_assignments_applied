
library(ISLR)

names(Carseats)

set.seed(1)
train = sample(1:nrow(Carseats),nrow(Carseats)/2)

library(tree)
carseats.tree = tree(Sales~.,data=Carseats,subset=train)

plot(carseats.tree)
text(carseats.tree,pretty=0)

#Finding the mean squared error
carseats.pred = predict(carseats.tree,newdata=Carseats[-train,])
mean((carseats.pred-Carseats$Sales[-train])^2)

cv.carseats.tree = cv.tree(carseats.tree)
cv.carseats.tree

plot(cv.carseats.tree$size,cv.carseats.tree$dev,type="b")

#As we can see the tree with 12 terminal nodes gives the lowest deviance.
prune.carseats.tree = prune.tree(carseats.tree,best=12)
plot(prune.carseats.tree)
text(prune.carseats.tree)

yhat = predict(prune.carseats.tree,newdata=Carseats[-train,])
mean((yhat-Carseats$Sales[-train])^2)

library(randomForest)

# p = the number of variables considered at each split
p = ncol(Carseats)-1
bag.carseats = randomForest(Sales~.,data=Carseats,mtry=p,ntree=500,importance=TRUE,subset=train)

yhat = predict(bag.carseats,newdata=Carseats[-train,])
mean((yhat-Carseats$Sales[-train])^2)

varImpPlot(bag.carseats)

# p = the number variables considered at each split
p = sqrt(ncol(Carseats)-1)
p2 = (ncol(Carseats)-1)/2
randf.carseats = randomForest(Sales~.,data=Carseats,mtry=p,ntree=500,importance=TRUE,subset=train)
randf.carseats.p2 = randomForest(Sales~.,data=Carseats,mtry=p2,ntree=500,importance=TRUE,subset=train)

yhat = predict(randf.carseats,newdata=Carseats[-train,])
mean((yhat-Carseats$Sales[-train])^2)

yhat = predict(randf.carseats.p2,newdata=Carseats[-train,])
mean((yhat-Carseats$Sales[-train])^2)

importance(randf.carseats)
varImpPlot(randf.carseats)
