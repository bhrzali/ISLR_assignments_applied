
Auto = read.csv("../../datasets/Auto.csv",header=T,na.strings="?")
Auto = na.omit(Auto)

pairs(Auto)

auto_subset = subset(Auto, select=-name)
cor(auto_subset)

lm.fit = lm(mpg~.-name, data=Auto)
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

lm.fit_interaction = lm(mpg~.+cylinders*displacement+cylinders:year-name,data=Auto)
summary(lm.fit_interaction)

lm.fit_logY_transform = lm(log(mpg)~.+cylinders*displacement-name,data=Auto)
summary(lm.fit_logY_transform)

par(mfrow=c(2,2))
plot(lm.fit_logY_transform)

lm.fit_sqrtY_transform = lm(sqrt(mpg)~.+cylinders*displacement-name,data=Auto)
summary(lm.fit_logY_transform)
par(mfrow=c(2,2))
plot(lm.fit_sqrtY_transform)

lm.fit_Xsq = lm(mpg~horsepower+I(horsepower^2),data=Auto)
summary(lm.fit_Xsq)


