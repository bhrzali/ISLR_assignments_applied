
library(MASS)

summary(Boston)

lm.fit1 = lm(crim~zn,data=Boston)
summary(lm.fit1)

lm.fit2 = lm(crim~indus,data=Boston)
summary(lm.fit2)

chas2 = as.factor(Boston$chas) #making chas a qualitative variable
lm.fit3 = lm(crim~chas2,data=Boston)
summary(lm.fit3)

lm.fit4 = lm(crim~nox,data=Boston)
summary(lm.fit4)

lm.fit5 = lm(crim~rm,data=Boston)
summary(lm.fit5)

lm.fit6 = lm(crim~age,data=Boston)
summary(lm.fit6)

lm.fit7 = lm(crim~dis,data=Boston)
summary(lm.fit7)

lm.fit8 = lm(crim~rad,data=Boston)
summary(lm.fit8)

lm.fit9 = lm(crim~tax,data=Boston)
summary(lm.fit9)

lm.fit10 = lm(crim~ptratio,data=Boston)
summary(lm.fit10)

lm.fit11 = lm(crim~black,data=Boston)
summary(lm.fit11)

lm.fit12 = lm(crim~lstat,data=Boston)
summary(lm.fit12)

lm.fit13 = lm(crim~medv,data=Boston)
summary(lm.fit13)

Boston2 = Boston
Boston2[,4]=as.factor(chas)

summary(Boston2)

lm.fit_all = lm(crim~.,data=Boston2)
summary(lm.fit_all)

coef_simple = c(coef(lm.fit1)[-1],
                coef(lm.fit2)[-1],
                coef(lm.fit3)[-1],
                coef(lm.fit4)[-1],
                coef(lm.fit5)[-1],
                coef(lm.fit6)[-1],
                coef(lm.fit7)[-1],
                coef(lm.fit8)[-1],
                coef(lm.fit9)[-1],
                coef(lm.fit10)[-1],
                coef(lm.fit11)[-1],
                coef(lm.fit12)[-1],
                coef(lm.fit13)[-1])

coef_multi = coef(lm.fit_all)[-1]
plot(coef_simple, coef_multi)

for (p in 2:length(Boston)){
    pred = Boston[,p]
    if (colnames(Boston)[p]!="chas"){
        print(colnames(Boston)[p])
        lm.fit = lm(crim~poly(pred,3), data=Boston)
        print(summary(lm.fit))
    }
}


