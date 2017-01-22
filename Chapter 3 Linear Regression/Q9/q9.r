
Auto = read.csv("../../datasets/Auto.csv",header=T,na.strings="?")
Auto = na.omit(Auto)

pairs(Auto)

auto_subset = subset(Auto, select=-name)
cor(auto_subset)

lm.model = lm(mpg~.-name, data=Auto)
summary(lm.model)

par(mfrow=c(2,2))
plot(lm.model)

lm.model_interaction = lm(mpg~.+cylinders*displacement+cylinders:year-name,data=Auto)
summary(lm.model_interaction)

lm.model_logY_transform = lm(log(mpg)~.+cylinders*displacement-name,data=Auto)
summary(lm.model_logY_transform)

par(mfrow=c(2,2))
plot(lm.model_logY_transform)

lm.model_sqrtY_transform = lm(sqrt(mpg)~.+cylinders*displacement-name,data=Auto)
summary(lm.model_logY_transform)
par(mfrow=c(2,2))
plot(lm.model_sqrtY_transform)

lm.model_Xsq = lm(mpg~horsepower+I(horsepower^2),data=Auto)
summary(lm.model_Xsq)


