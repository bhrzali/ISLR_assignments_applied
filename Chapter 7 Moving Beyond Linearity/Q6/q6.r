
library(ISLR)

set.seed(1)
library(boot)
err = rep(NA,10)
for(i in 1:10){
    glm.model = glm(wage~poly(age,i),data=Wage)
    err[i]= cv.glm(Wage,glm.model,K=10)$delta[2]
}
plot(err,type="l")
min_point = which.min(err)
points(min_point,err[min_point],col="red")

lm.model1 = lm(wage~poly(age,1),data=Wage)
lm.model2 = lm(wage~poly(age,2),data=Wage)
lm.model3 = lm(wage~poly(age,3),data=Wage)
lm.model4 = lm(wage~poly(age,4),data=Wage)
lm.model5 = lm(wage~poly(age,5),data=Wage)
lm.model6 = lm(wage~poly(age,6),data=Wage)
lm.model7 = lm(wage~poly(age,7),data=Wage)
lm.model8 = lm(wage~poly(age,8),data=Wage)
lm.model9 = lm(wage~poly(age,9),data=Wage)
lm.model10 = lm(wage~poly(age,10),data=Wage)
anova(lm.model1,lm.model2,lm.model3,lm.model4,lm.model5,lm.model6,lm.model7,lm.model8,lm.model9,lm.model10)

age.limits = range(Wage$age)
age.range = seq(age.limits[1],age.limits[2])
glm.model = glm(wage~poly(age,4),data=Wage)
glm.pred = predict(glm.model,newdata=list(age=age.range))
plot(Wage$age,Wage$wage,col="darkgrey")
lines(age.range,glm.pred,col="blue",lwd=2)

#Using k-fold cross validation using a manual method. (Refer below for R's inbuit function for cross validation)
library(ISLR)
set.seed(1)
k=10
folds = sample(1:k,nrow(Wage),replace=TRUE)
cv.err = c()
for(cp in 2:10){
    err = c()
    for(i in 1:k){
        Wage$cuts = cut(Wage$age,cp)
        lm.model = lm(wage~cuts,data=Wage[folds!=i,])
        lm.pred = predict(lm.model,newdata=Wage[folds==i,])
        err[i]=mean((lm.pred-Wage[folds==i,]$wage)^2)
    }
    cv.err[cp]=mean(err)
}
plot(cv.err,type="l")

#The above k-fold cross validation can also be done with R's inbuilt function
cv.err = c()
for(i in 2:10){
    Wage$cuts = cut(Wage$age,i)
    glm.model = glm(wage~cuts, data=Wage)
    cv.err[i]=cv.glm(Wage,glm.model,K=10)$delta[2]
}
plot(cv.err,type="l")

lm.model = lm(wage~cut(age,8),data=Wage)
lm.pred = predict(lm.model,newdata=list(age=age.range))
plot(Wage$age,Wage$wage,col="darkgrey")
lines(age.range,lm.pred,col="blue",lwd=2)


