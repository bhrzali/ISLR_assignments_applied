
library(ISLR)

summary(Auto)

Auto = na.omit(Auto)
mpg.bin = rep(0,length(Auto$mpg))
mpg.bin[Auto$mpg>median(Auto$mpg)]=1

set.seed(1)
library(e1071)
Auto.copy = Auto[,c(-1,-9)]
Auto.copy$mpgbin = as.factor(mpg.bin)
range = list(cost=c(0.001,0.01,1,10,100,1000,10000))
tune.linear = tune(svm,mpgbin~.,data=Auto.copy,kernel="linear",ranges=range)
summary(tune.linear)

set.seed(435)
r = list(cost=c(0.001,0.1,1,10,100),degree=c(2,3,4,5))
tune.poly = tune(svm,mpgbin~.,data=Auto.copy,kernel="polynomial",ranges=r)
summary(tune.poly)

set.seed(435)
r = list(cost=c(0.001,0.1,1,10,100),gamma=c(1,2,3,4))
tune.radial = tune(svm,mpgbin~.,data=Auto.copy,kernel="radial",ranges=r)
summary(tune.radial)

names=names(Auto.copy)
names

names = names[c(-4,-8)]

for(name in names){
    plot(tune.linear$best.model,Auto.copy,as.formula(paste("weight~",name,sep="")))
}

for(name in names){
    plot(tune.poly$best.model,Auto.copy,as.formula(paste("weight~",name,sep="")))
}

for(name in names){
    plot(tune.linear$best.model,Auto.copy,as.formula(paste("weight~",name,sep="")))
}


