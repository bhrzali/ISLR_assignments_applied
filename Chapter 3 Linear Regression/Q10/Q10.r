
library(ISLR)

Carseats = na.omit(Carseats)
summary(Carseats)

lm.fit = lm(Sales~Price+Urban+US,data=Carseats)
summary(lm.fit)

lm.fit_small = lm(Sales~Price+US,data=Carseats)
summary(lm.fit_small)

confint(lm.fit_small, level=0.95)

plot(predict(lm.fit_small),rstudent(lm.fit_small))

par(mfrow=c(2,2))
plot(lm.fit_small)

p=2
n=nrow(Carseats)
(p+1)/n
