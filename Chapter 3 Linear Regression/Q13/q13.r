
set.seed(1)

x = rnorm(100,mean=0,sd=1)

eps = rnorm(100,mean=0,sd=sqrt(0.25))

y = -1+0.5*x+eps

length(y)

plot(y,x)

lm.fit = lm(y~x)
summary(lm.fit)

plot(x,y)
abline(lm.fit,col="red")
abline(-1,0.5,col="blue")
legend(-2,0.5, legend = c("least square fit", "pop. regression"), col=c("red","blue"), lwd=2)

lm.fit_quad = lm(y~x+I(x^2))
summary(lm.fit_quad)

anova(lm.fit,lm.fit_quad)
According to the anova test the p-value of the f-statistic is larger than 5%. Hence the null hypothesis stands true that both models fit the data equally well.

Moreover the p-value of the t-statistic of the coefficient of I(x^2) is larger than 5%. Therefore the null hypothesis is stands true that this coefficient is equal to zero and can be omited.
set.seed(1)
x2 = rnorm(100,mean=0,sd=1)
eps2 = rnorm(100,mean=0,sd=sqrt(0.1))
y2 = -1+0.5*x2+eps2

plot(x2,y2)

lm.fit2 = lm(y2~x2)
summary(lm.fit2)

plot(x2,y2)
abline(lm.fit2, col="red")
abline(-1,0.5, col="blue")
legend(-2,0.0, legend=c("least square fit","pop. regression"), col=c("red","blue"),lwd=2)

lm.fit2_quad = lm(y2~x2+I(x2^2))
summary(lm.fit2_quad)

anova(lm.fit2,lm.fit2_quad)

set.seed(1)
x3 = rnorm(100,mean=0,sd=1)
eps3 = rnorm(100,mean=0,sd=sqrt(0.6))
y3 = -1+0.5*x3+eps3
lm.fit3 = lm(y3~x3)
summary(lm.fit3)
plot(x3,y3)
abline(lm.fit3, col="red")
abline(-1,0.5, col="blue")
legend(-2,0.0, legend=c("least square fit","pop. regression"), col=c("red","blue"),lwd=2)

lm.fit3_quad = lm(y3~x3+I(x3^2))
summary(lm.fit3_quad)
anova(lm.fit3,lm.fit3_quad)

#Original dataset
confint(lm.fit)

#Less noisy dataset
confint(lm.fit2)

#Noiser dataset
confint(lm.fit3)


