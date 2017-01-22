
set.seed(1)

x = rnorm(100,mean=0,sd=1)

er = rnorm(100,mean=0,sd=sqrt(0.25))

y = -1+0.5*x+er

length(y)

plot(y,x)

lm.model = lm(y~x)
summary(lm.model)

plot(x,y)
abline(lm.model,col="red")
abline(-1,0.5,col="blue")
legend(-2,0.5, legend = c("least square fit", "pop. regression"), col=c("red","blue"), lwd=2)

lm.model_quad = lm(y~x+I(x^2))
summary(lm.model_quad)

anova(lm.model,lm.model_quad)
According to the anova test the p-value of the f-statistic is larger than 5%. Hence the null hypothesis stands true that both models fit the data equally well.

Moreover the p-value of the t-statistic of the coefficient of I(x^2) is larger than 5%. Therefore the null hypothesis is stands true that this coefficient is equal to zero and can be omited.
set.seed(1)
x2 = rnorm(100,mean=0,sd=1)
er2 = rnorm(100,mean=0,sd=sqrt(0.1))
y2 = -1+0.5*x2+er2

plot(x2,y2)

lm.model2 = lm(y2~x2)
summary(lm.model2)

plot(x2,y2)
abline(lm.model2, col="red")
abline(-1,0.5, col="blue")
legend(-2,0.0, legend=c("least square fit","pop. regression"), col=c("red","blue"),lwd=2)

lm.model2_quad = lm(y2~x2+I(x2^2))
summary(lm.model2_quad)

anova(lm.model2,lm.model2_quad)

set.seed(1)
x3 = rnorm(100,mean=0,sd=1)
er3 = rnorm(100,mean=0,sd=sqrt(0.6))
y3 = -1+0.5*x3+er3
lm.model3 = lm(y3~x3)
summary(lm.model3)
plot(x3,y3)
abline(lm.model3, col="red")
abline(-1,0.5, col="blue")
legend(-2,0.0, legend=c("least square fit","pop. regression"), col=c("red","blue"),lwd=2)

lm.model3_quad = lm(y3~x3+I(x3^2))
summary(lm.model3_quad)
anova(lm.model3,lm.model3_quad)

#Original dataset
confint(lm.model)

#Less noisy dataset
confint(lm.model2)

#Noiser dataset
confint(lm.model3)


