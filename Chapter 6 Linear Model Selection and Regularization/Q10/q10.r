
set.seed(1)
#creating coefficient vector
#beta=sample(1:20,20,replace=TRUE)
beta=rnorm(20)
beta[sample(1:20,5)]=0
#creating X variables matrix
n=1000
p=20
X = matrix(rnorm(n*p),n,p)
#error
err = rnorm(n)
#model
Y = X%*%beta + err
#dataset
dataset=data.frame(Y,X)

set.seed(1)
train = sample(nrow(dataset),100,replace=FALSE)

library(leaps)
set.seed(1)
bestsubset.model = regsubsets(Y~.,data=dataset,nvmax=20,subset=train)
bestsubset.summary = summary(bestsubset.model)

bestsubset.summary

mse=rep(NA,20)
for(i in 1:20){
    coef_vec = coef(bestsubset.model,id=i)
    pred=as.matrix(dataset[train,names(coef_vec)[-1]])%*%coef_vec[-1]
    mse[i]=mean((pred-dataset$Y[train])^2)
}

plot(mse,ylab="mean squared error",xlab="number of variables",type="b",main="Training MSE")

mse=rep(NA,20)
for(i in 1:20){
    coef_vec = coef(bestsubset.model,id=i)
    pred=as.matrix(dataset[-train,names(coef_vec)[-1]])%*%coef_vec[-1]
    mse[i]=mean((pred-dataset$Y[-train])^2)
}

plot(mse,ylab="mean squared error",xlab="number of variables",type="b",main="Test MSE")

#The size of the model giving the least MSE on test data
which.min(mse)

which(beta==0)
coef(bestsubset.model,id=13)

b=beta
b.hat = beta
b.hat = data.frame(b)
rownames(b.hat)=names(coef(bestsubset.model,id=20))[-1]
val = rep(NA,20)
for(i in 1:20){
    coef_vec = coef(bestsubset.model,id=i)
    match  = rownames(b.hat) %in% names(coef_vec)
    b.hat[match,]=coef_vec[-1]
    b.hat[!match,]=0
    val[i]=sqrt(sum((b-b.hat)^2))
}
plot(val,ylab="sqrt of sum of squared error of coefficients",xlab="no. of variables",type="b")

which.min(val)


