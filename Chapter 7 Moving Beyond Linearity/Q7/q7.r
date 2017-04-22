
library(ISLR)
summary(Wage)

par(mfrow=c(2,2))
plot(Wage$jobclass,Wage$wage)
plot(Wage$maritl,Wage$wage)
plot(Wage$health_ins,Wage$wage)

library(gam)
gam.model = gam(wage~maritl+jobclass+health_ins,data=Wage)
par(mfrow=c(2,2))
plot(gam.model)


