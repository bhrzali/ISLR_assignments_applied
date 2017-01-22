
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1+rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)

cor(x1,x2)
plot(x1,x2)

lm.model1 = lm(y~x1+x2)
summary(lm.model1)

lm.model2 = lm(y~x1)
summary(lm.model2)

lm.model3 = lm(y~x2)
summary(lm.model3)

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y,6)

lm.model_g = lm(y~x1+x2)
summary(lm.model_g)
par(mfrow=c(2,2))
plot(lm.model_g)

p=2
n = length(y)
(p+1)/n

lm.model_g2 = lm(y~x1)
summary(lm.model_g2)
par(mfrow=c(2,2))
plot(lm.model_g2)
par(mfrow=c(1,1))
plot(predict(lm.model_g2),rstudent(lm.model_g2))

lm.model_g3 = lm(y~x2)
summary(lm.model_g3)
par(mfrow=c(2,2))
plot(lm.model_g3)
par(mfrow=c(1,1))
plot(predict(lm.model_g3),rstudent(lm.model_g3))


