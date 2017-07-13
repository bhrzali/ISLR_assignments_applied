
set.seed(1)
class = sample(100,50)
x = rnorm(200)
x2 = 2*x^2 + 3*x + 4 + rnorm(200)
x2[class]=x2[class]+3
x2[-class]=x2[-class]-3
y=rep(0,200)
y[class]=1
y[-class]=2
dat = data.frame(x=cbind(x,x2),y=as.factor(y))
plot(dat[,-3],col=y)

train = sample(200,100)

#linear kernel
svmfit.l = svm(y~.,data=dat[train,],kernel="linear",cost=10)
plot(svmfit.l,dat[train,])

#polynomial kernel
svmfit.p = svm(y~.,data=dat[train,],kernel="polynomial",cost=3)
plot(svmfit.p,dat[train,])

#radial kernel
svmfit.r = svm(y~.,data=dat[train,],kernel="radial",cost=10)
plot(svmfit.r,dat[train,])

#linear kernel
pred = predict(svmfit.l,newdata=dat[-train,])
table(pred,dat[-train,]$y)

#polynomial kernel
pred=predict(svmfit.p,newdata=dat[-train,])
table(pred,dat[-train,]$y)

#radial kernel
pred=predict(svmfit.r,newdata=dat[-train,])
table(pred,dat[-train,]$y)


