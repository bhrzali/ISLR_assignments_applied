
library(ISLR)

summary(OJ)

set.seed(1)
train = sample(1:nrow(OJ),800)

library(tree)
oj.model = tree(Purchase~.,data=OJ,subset=train)
summary(oj.model)

oj.model

plot(oj.model)
text(oj.model)

yhat = predict(oj.model,newdata=OJ[-train,],type="class")
table(yhat,OJ$Purchase[-train])

mean((yhat!=OJ$Purchase[-train]))*100

cv.oj.model = cv.tree(oj.model)
cv.oj.model

plot(cv.oj.model$size,cv.oj.model$dev,xlab="size",ylab="deviance",type="b")

pruned.oj.model = prune.tree(oj.model,best=6)
plot(pruned.oj.model)
text(pruned.oj.model)

summary(pruned.oj.model)

summary(oj.model)

#test error rate of unpruned tree
yhat = predict(oj.model,newdata=OJ[-train,],type="class")
mean(yhat!=OJ$Purchase[-train])*100

#test error rate of pruned tree
yhat = predict(pruned.oj.model,newdata=OJ[-train,],type="class")
mean(yhat!=OJ$Purchase[-train])*100
