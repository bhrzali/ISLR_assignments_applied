
library(ISLR)
College = na.omit(College)
summary(College)

#Creating a training data
set.seed(1)
size = nrow(College)
train = sample(1:size,size%/%2)

lm.model = lm(Apps~.,data=College,subset=train)
summary(lm.model)

lm.pred = predict(lm.model,newdata=College[-train,])
#mean squared error of least squares error linear regression
mean((College[-train,]$Apps-lm.pred)^2)

#Ridge regression
set.seed(1)
library(glmnet)
X = model.matrix(Apps~.,data=College)[,-1]
Y = College$Apps
ridge.model = cv.glmnet(X[train,],Y[train],alpha=0)
best.lambda=ridge.model$lambda.min
best.lambda

coef.pred = predict(ridge.model,type="coefficients",s=best.lambda)
coef.pred

ridge.pred = predict(ridge.model,newx=X[-train,],s=best.lambda)
#mean squared error of ridge regression
mean((ridge.pred-College[-train,]$Apps)^2)

#Lasso
set.seed(1)
lasso.model = cv.glmnet(X[train,],Y[train],alpha=1)
best.lambda=lasso.model$lambda.min
best.lambda

coef.pred = predict(lasso.model,type="coefficients",s=best.lambda)
coef.pred

#number of non-zero coefficient estimates
length(coef.pred[coef.pred!=0])

#mean squared test error
lasso.pred = predict(lasso.model,newx=X[-train,],s=best.lambda)
mean((lasso.pred-Y[-train])^2)

#Principal Component Analysis
library(pls)
pcr.model=pcr(Apps~.,data=College[train,],scale=TRUE,validation="CV")
validationplot(pcr.model,val.type="MSEP")

summary(pcr.model)

#mean squared error pcr
pcr.pred = predict(pcr.model,newdata=College[-train,],ncomp=16)
mean((pcr.pred-College[-train,]$Apps)^2)

#Partial Least Square
pls.model = plsr(Apps~.,data=College[train,],scale=TRUE,validation="CV")
validationplot(pls.model,val.type="MSEP")

summary(pls.model)

#mean squared error on test data for pls
pls.pred = predict(pls.model,newdata=College[-train,],ncomp=11)
mean((pls.pred-College[-train,]$Apps)^2)

#R-squared
y = College[-train,]$Apps
#ols
lm.rsq = 1-sum((College[-train,]$Apps-lm.pred)^2)/sum((y-mean(y))^2)
#Ridge
ridge.rsq = 1-sum((College[-train,]$Apps-ridge.pred)^2)/sum((y-mean(y))^2)
#Lasso
lasso.rsq = 1-sum((College[-train,]$Apps-lasso.pred)^2)/sum((y-mean(y))^2)
#pcr
pcr.rsq = 1-sum((College[-train,]$Apps-pcr.pred)^2)/sum((y-mean(y))^2)
#pls
pls.rsq = 1-sum((College[-train,]$Apps-pls.pred)^2)/sum((y-mean(y))^2)
#barplot of R-square of all models on test data.
barplot(c(lm.rsq,ridge.rsq,lasso.rsq,pcr.rsq,pls.rsq),col="blue",names.arg=c("OLS","Ridge","Lasso","PCR","PLS"))

#R-square values
lm.rsq
ridge.rsq
lasso.rsq
pcr.rsq
pls.rsq


