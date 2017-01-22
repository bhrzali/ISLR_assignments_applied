
set.seed(1)
x = rnorm(100,mean=0,sd=1)
eps = rnorm(100,mean=0,sd=sqrt(0.25))
y = 4*x+eps
plot(x,y)

lm.model1 = lm(y~x+0)
summary(lm.model1)

lm.model2 = lm(x~y+0)
summary(lm.model2)

x = rnorm(100,mean=0,sd=1)
y = sample(x,100)

#output True if both are equal
sum(x^2)==sum(y^2)

lm.model1 = lm(y~x+0)
summary(lm.model1)

lm.model2 = lm(x~y+0)
summary(lm.model2)


