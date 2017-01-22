
library(ISLR)

Carseats = na.omit(Carseats)
summary(Carseats)

lm.model = lm(Sales~Price+Urban+US,data=Carseats)
summary(lm.model)

lm.model_small = lm(Sales~Price+US,data=Carseats)
summary(lm.model_small)

anova(lm.model,lm.model_small)

confint(lm.model_small, level=0.95)

plot(predict(lm.model_small),rstudent(lm.model_small))

par(mfrow=c(2,2))
plot(lm.model_small)

p=2
n=nrow(Carseats)
(p+1)/n
