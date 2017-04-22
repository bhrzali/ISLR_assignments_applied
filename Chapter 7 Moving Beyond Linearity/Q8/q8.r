
library(ISLR)
summary(Auto)

plot(Auto)

library(boot)
cv.err = c()
set.seed(1)
Auto = na.omit(Auto)
for(i in 1:6){
    glm.model = glm(mpg~poly(weight,i),data=Auto)
    cv.err[i] = cv.glm(Auto,glm.model,K=10)$delta[2]
}
plot(cv.err,type="l",xlab="no. of degrees",ylab="cv err")
title("10-fold Cross Validation Graph")

weight.limits = range(Auto$weight)
weight.grid = seq(weight.limits[1],weight.limits[2])
glm.model = glm(mpg~poly(weight,2),data=Auto)
pred = predict(glm.model,newdata=list(weight=weight.grid))
plot(Auto$weight,Auto$mpg,col="darkgrey",xlab="weight",ylab="mpg")
title("mpg vs weight graph")
lines(weight.grid,pred,col="blue")

library(splines)
err = c()
set.seed(1)
for(i in 1:10){
    ns.model = glm(mpg~ns(weight,i),data=Auto)
    err[i]=cv.glm(Auto,ns.model,K=10)$delta[2]
}
err

ns.model = lm(mpg~ns(weight,3),data=Auto)
pred = predict(ns.model,newdata=list(weight=weight.grid))
plot(Auto$weight,Auto$mpg,col="darkgrey",xlab="weight",ylab="mpg")
title("mpg vs weight graph")
lines(weight.grid,pred,col="blue")

#We will let the function choose the degree of freedom
smooth.model = smooth.spline(Auto$weight,Auto$mpg,cv=TRUE)
smooth.model$df

plot(Auto$weight,Auto$mpg,col="darkgrey",xlab="weight",ylab="mpg")
title("mpg vs weight graph")
lines(smooth.model,col="blue")

library(gam)
gam.model = gam(mpg~ns(weight,3)+ns(displacement,3),data=Auto)
deviance(ns.model) #deviance of natural spline
deviance(gam.model) #deviance of Generalized additive model


