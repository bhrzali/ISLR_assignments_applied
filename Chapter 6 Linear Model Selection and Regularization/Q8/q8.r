
set.seed(1)
X = rnorm(100)
error = rnorm(100)

b0=3
b1=2
b2=-4
b3=0.4
Y = b0+b1*X+b2*X^2+b3*X^3+error
sim_data = data.frame(y=Y,x=X)

library(leaps)
bestsubset.fit = regsubsets(Y~poly(X,10,raw=T),data=sim_data,nvmax=10)
bestsubset_summary = summary(bestsubset.fit)
bestsubset_summary

names(bestsubset_summary)

par(mfrow=c(2,2))
#adjusted R-squared
plot(bestsubset_summary$adjr2,xlab="Number of variables",ylab="adjr2",type="l")
max_adjr2 = which.max(bestsubset_summary$adjr2)
points(max_adjr2,bestsubset_summary$adjr2[max_adjr2],col="red",cex=2,pch=20)
#cp
plot(bestsubset_summary$cp,xlab="Number of Variables",ylab="cp",type="l")
min_cp = which.min(bestsubset_summary$cp)
points(min_cp,bestsubset_summary$cp[min_cp],col="red",cex=2,pch=20)
#bic
plot(bestsubset_summary$bic,xlab="Number of Variables",ylab="bic",type="l")
min_bic = which.min(bestsubset_summary$bic)
points(min_bic,bestsubset_summary$bic[min_bic],col="red",cex=2,pch=20)

max_adjr2
min_bic
min_cp

coef(bestsubset.fit,3)

#forward stepwise selection
forward.fit = regsubsets(Y~poly(x,10,raw=T),data=sim_data,nvmax=10,method="forward")
forward_summary = summary(forward.fit)
forward_summary

max_adjr2 = which.max(forward_summary$adjr2)
max_adjr2

min_bic = which.min(forward_summary$bic)
min_bic

min_cp = which.min(forward_summary$cp)
min_cp

coef(forward.fit,3)

#backward stepwise regression
backward.fit = regsubsets(Y~poly(x,10,raw=T),data=sim_data,nvmax=10,method="backward")
backward_summary = summary(backward.fit)
backward_summary

which.max(backward_summary$adjr2)

which.min(backward_summary$bic)

which.min(backward_summary$cp)

coef(backward.fit,3)

X = model.matrix(Y~poly(x,10,raw=T),data=sim_data)[,-1]
Y = sim_data[,1]
#creating a trianing data
set.seed(1)
train = sample(1:100,50)

set.seed(1)
cv.output = cv.glmnet(X[train,],Y[train],alpha=1)
plot(cv.output)

best.lambda = cv.output$lambda.min
lasso.fit = glmnet(X,Y,alpha=1)
coef.pred = predict(lasso.fit,type="coefficients",s=best.lambda)[1:11,]
coef.pred

coef.pred[coef.pred!=0]

b0=4
b7=8
Y = b0 + b7*X^7 + error
sim_data = data.frame(y=Y,x=X)

#using best subset selection
best.subset.fit = regsubsets(Y~poly(x,10,raw=T),data=sim_data,nvmax=10)
best.subset.summary = summary(best.subset.fit)
names(best.subset.summary)

which.min(best.subset.summary$cp)

which.min(best.subset.summary$bic)

which.max(best.subset.summary$adjr2)

coef(best.subset.fit,2)

coef(best.subset.fit,1)

coef(best.subset.fit,4)

#using lasso
b0=4
b7=8
Y = b0 + b7*X^7 + error
sim_data = data.frame(y=Y,x=X)
X = model.matrix(Y~poly(x,10,raw=T),data=sim_data)[,-1]
Y = sim_data[,1]
#creating training data
set.seed(1)
train = sample(1:100,50)

cv.output = cv.glmnet(X[train,],Y[train],alpha=1)
best.lambda=cv.output$lambda.min
best.lambda

lasso.fit = glmnet(X,Y,alpha=1)
coef.pred = predict(lasso.fit,type="coefficients",s=best.lambda)[1:11,]
coef.pred

coef.pred[coef.pred!=0]
