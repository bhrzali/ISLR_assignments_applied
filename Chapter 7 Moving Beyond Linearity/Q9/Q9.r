
library(ISLR)
library(MASS)
summary(Boston)

lm.fit = lm(nox~poly(dis,3),data=Boston)
dis.lims = range(Boston$dis)
dis.grid = seq(dis.lims[1],dis.lims[2],0.1)
pred = predict(lm.fit,newdata=list(dis=dis.grid))

plot(Boston$dis,Boston$nox,col="darkgrey",xlab="weighted distance", ylab="nox ppm")
lines(dis.grid,pred,col="blue")

par(mfrow=c(4,4))
rss = c()
for(d in 1:10){
    lm.fit = lm(nox~poly(dis,d),data=Boston)
    pred = predict(lm.fit,newdata=list(dis=dis.grid))
    plot(Boston$dis,Boston$nox,col="darkgrey",xlab="weighted distance", ylab="nox ppm")
    lines(dis.grid,pred,col="blue")
    rss[d]=deviance(lm.fit)
}

#RSS
print(rss)

library(boot)
set.seed(3)
cverr = c()
for(i in 1:10){
    glm.model = glm(nox~poly(dis,i),data=Boston)
    cverr[i] = cv.glm(Boston,glm.model,K=10)$delta[2]
}
plot(cverr,type="l",xlab="no. of degrees")
min_point = which.min(cverr)
points(min_point,cverr[min_point],col="red")

# No. of degrees of polynomial giving the least cross validation error
min_point

library(splines)
spline.model = lm(nox~bs(dis,df=4),data=Boston)
pred = predict(spline.model,newdata=list(dis=dis.grid))
plot(Boston$dis,Boston$nox,col="darkgrey",xlab="weighted distance", ylab="nox ppm")
lines(dis.grid,pred,col="blue")

rss=c()
par(mfrow=c(3,3))
for(i in 3:10){
    spline.model = lm(nox~bs(dis,i),data=Boston)
    pred = predict(spline.model,newdata=list(dis=dis.grid))
    plot(Boston$dis,Boston$nox,col="darkgrey",xlab="weighted distance", ylab="nox ppm")
    lines(dis.grid,pred,col="blue")
    rss[i]=deviance(spline.model)
}

#rss
rss

cverr = c()
set.seed(1)
for(i in 3:10){
    spline.model = glm(nox~bs(dis,i),data=Boston)
    cverr[i] = cv.glm(Boston,spline.model,K=10)$delta[2]
}
plot(cverr,type="l")


