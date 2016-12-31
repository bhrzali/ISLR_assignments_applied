
Auto= read.csv("../../datasets/Auto.csv",header=T,na.strings="?")
Auto=na.omit(Auto)

lm.fit = lm(mpg~horsepower, data=Auto)
summary(lm.fit)

mean(Auto$mpg)

predict(lm.fit,data.frame(horsepower=98))

#95% confidence interval
predict(lm.fit,data.frame(horsepower=98),interval="confidence")

#95% prediction interval
predict(lm.fit,data.frame(horsepower=98),interval="prediction")

plot(Auto$horsepower, Auto$mpg)
abline(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)


