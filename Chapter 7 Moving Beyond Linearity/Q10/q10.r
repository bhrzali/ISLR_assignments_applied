
library(ISLR)
summary(College)

train = sample(1:nrow(College)%/%2)
length(names(College))

library(leaps)
regfit.forward = regsubsets(Outstate~.,data=College,nvmax=18,method="forward")
regfit_summary = summary(regfit.forward)
regfit_summary

names(regfit_summary)

par(mfrow=c(2,2))
plot(regfit_summary$adjr2,type="l",ylab="adjusted R.sq", xlab="no. of predictors")
max_adjr2 = which.max(regfit_summary$adjr2)
points(max_adjr2,regfit_summary$adjr2[max_adjr2],col="red")
plot(regfit_summary$bic,type="l",ylab="BIC", xlab="no. of predictors")
min_bic = which.min(regfit_summary$bic)
points(min_bic,regfit_summary$bic[min_bic],col="red")
plot(regfit_summary$cp,type="l",ylab="CP", xlab="no. of predictors")
min_cp = which.min(regfit_summary$cp)
points(min_cp,regfit_summary$cp[min_cp],col="red")

k=10
folds = sample(1:k,nrow(College),replace=TRUE)

set.seed(1)
cverr = c()
for(i in 1:17){
    err = c()
    for(j in 1:k){
        regfit_model = regsubsets(Outstate~.,data=College[folds!=k,],method="forward")
        test_mat = model.matrix(Outstate~.,data=College[folds==k,])
        coef_vec = coef(regfit.forward,id=i)
        n = names(coef_vec)
        pred = test_mat[,n]%*%coef_vec
        err[j]=mean((pred-College[folds==k,]$Outstate)^2)
    }
    cverr[i]=mean(err)
}
plot(cverr,type="l",xlab="no. of predictors")

which.min(cverr)

coef(regfit.forward,id=10)

names(College)

library(gam)
library(splines)
gam.model = gam(Outstate~Private+s(Accept,df=3)+s(Room.Board,df=3)+s(Books,df=3)+s(Personal,df=3)+s(PhD,df=3)+
                s(S.F.Ratio,df=3)+s(perc.alumni,df=3)+s(Expend,df=3)+s(Grad.Rate,df=3), data=College[train,])

par(mfrow=c(4,4))
plot(gam.model)

pred = predict(gam.model,newdata=College[-train,])

#R-squared
TSS = sum((College[-train,]$Outstate-mean(College[-train,]$Outstate))^2)
RSS = sum((College[-train,]$Outstate-pred)^2)
1-RSS/TSS

summary(gam.model)


