
Auto= read.csv("../../datasets/Auto.csv",header=T,na.strings="?")
Auto=na.omit(Auto)

lm.model = lm(mpg~horsepower, data=Auto)
summary(lm.model)

mean(Auto$mpg)

predict(lm.model,data.frame(horsepower=98))

#95% confidence interval
predict(lm.model,data.frame(horsepower=98),interval="confidence")

#95% prediction interval
predict(lm.model,data.frame(horsepower=98),interval="prediction")

plot(Auto$horsepower, Auto$mpg)
abline(lm.model)

par(mfrow=c(2,2))
plot(lm.model)


