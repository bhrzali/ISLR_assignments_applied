
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

lm.model = lm(y~x+0)
summary(lm.model)

plot(x,y)
abline(lm.model)

summary.lm(lm.model)$coefficients

lm.model_x_y = lm(x~y+0)
summary(lm.model_x_y)

summary.lm(lm.model_x_y)$coefficients

n = length(x)
X = matrix(data=x, nrow=n, ncol=1)
Y = matrix(data=y, nrow=n, ncol=1)
t = (sqrt(n-1)*sum(X*Y))/sqrt(sum(X^2)*sum(Y^2)-(sum(X*Y))^2)
print(t)

lm.model1 = lm(y~x)
summary(lm.model1)

lm.model2 = lm(x~y)
summary(lm.model2)
