
set.seed(1)
x1 = runif(500)-0.5
x2 = runif(500)-0.5
y=1*((x1^2-x2^2)>0)

dat = cbind(x1,x2)
positive.class = dat[as.factor(y)==1,]
negative.class = dat[as.factor(y)==0,]
plot(positive.class,col="blue",pch="+")
points(negative.class,col="red",pch="-")

#dat = data.frame(x=cbind(x1,x2),y=as.factor(y))
lgsfit = glm(as.factor(y)~x1+x2,family=binomial)
summary(lgsfit)

data = data.frame(cbind(x1,x2))
prob = predict(lgsfit,newdata=data,type="response")
pred = rep(0,length(prob))
pred[prob>=0.5]=1

positive.class = data[pred==1,]
negative.class = data[pred==0,]
plot(negative.class,col="red",pch="-")
points(positive.class,col="blue",pch="+")

glmfit = glm(y~poly(x1,2)+I(x1*x2)+poly(x2,2),family=binomial)
summary(glmfit)

data = data.frame(cbind(x1,x2))
prob = predict(glmfit,newdata=data,type="response")
pred = rep(0,length(prob))
pred[prob>=0.5]=1
positive.class = data[pred==1,]
negative.class = data[pred==0,]
plot(positive.class,col="blue",pch="+")
points(negative.class,col="red",pch="-")

library(e1071)
svmfit = svm(as.factor(y)~x1+x2,kernel="linear",cost=0.1)
data = cbind(x1,x2)
pred = predict(svmfit,data)
positive.class = data[pred==1,-3]
negative.class = data[pred==0,-3]
plot(negative.class,col="red",pch="-")
points(positive.class,col="blue",pch="+")

svmfit = svm(as.factor(y)~x1+x2,kernel="radial",gamma=1)
data = cbind(x1,x2)
pred = predict(svmfit,data)
positive.class=data[pred==1,]
negative.class=data[pred==0,]
plot(positive.class,col="blue",pch="+")
points(negative.class,col="red",pch="-")


