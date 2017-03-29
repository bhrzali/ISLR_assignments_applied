
library(MASS)
summary(Boston)
Boston=na.omit(Boston)

library(leaps)
set.seed(1)
k=10
folds=sample(1:k,nrow(Boston),replace=TRUE)
cv.err = matrix(NA,nrow=k,ncol=13)
for(i in 1:k){
    bestsubset.model = regsubsets(crim~.,data=Boston[folds!=i,],nvmax=13)
    for(j in 1:13){
        form = as.formula(bestsubset.model$call[[2]])
        dataset = model.matrix(form,Boston[folds==i,])
        coef_vec = coef(bestsubset.model,id=j)
        pred = dataset[,names(coef_vec)]%*%coef_vec
        cv.err[i,j]=mean((pred-Boston$crim[folds==i])^2)
    }
}
mse = apply(cv.err,MARGIN=2,mean)

plot(mse,type="b",xlab="no. of variables")

which.min(mse)

bestsubset.mse = mse[which.min(mse)]
bestsubset.mse

library(glmnet)
set.seed(1)
x = model.matrix(crim~.,data=Boston)[,-1]
y = Boston$crim
train = sample(nrow(x),nrow(x)%/%2)
cv.lasso = cv.glmnet(x[train,],y[train],alpha=1,type="mse")
plot(cv.lasso)
best.lambda = cv.lasso$lambda.min
best.lambda

lasso.model = glmnet(x,y,alpha=1)
coef = predict(lasso.model,type="coefficient",s=best.lambda)
coef

#minimum squared error for lasso model
lasso.mse = min(cv.lasso$cvm)
lasso.mse

cv.ridge = cv.glmnet(x[train,],y[train],alpha=0)
best.lambda = cv.ridge$lambda.min
best.lambda
plot(cv.ridge)

ridge.mod = glmnet(x,y,alpha=0)
coef = predict(ridge.mod,type="coefficient",s=best.lambda)
coef

#minimum squared error of ridge regression
ridge.mse = min(cv.ridge$cvm)
ridge.mse

library(pls)
pcr.model = pcr(crim~.,data=Boston[train,],scale=TRUE,validation="CV")
validationplot(pcr.model,val.type="MSEP")

pcr.pred = predict(pcr.model,newdata=Boston[-train,],ncomp=10)
mean((pcr.pred-Boston[-train,]$crim)^2)

summary(pcr.model)

#minimum suared error of PCR
pcr.mse = 6.978^2
pcr.mse

barplot(c(bestsubset.mse,lasso.mse,ridge.mse,pcr.mse),col="blue",names.arg=c("bestsubset","lasso","ridge","pcr"))


