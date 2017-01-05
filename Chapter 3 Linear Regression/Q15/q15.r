
library(MASS)

summary(Boston)

Boston2 = Boston
coef_simple = c()
Boston2$chas = as.factor(Boston$chas)
for (p in 2:length(Boston2)){
    pred = Boston[,p]
    print(colnames(Boston2)[p])
    lm.fit = lm(crim~pred,data=Boston)
    coef_simple = c(coef_simple, coef(lm.fit)[-1]) # for part (c)
    print(summary(lm.fit))
}

summary(Boston2)

lm.fit_all = lm(crim~.,data=Boston2)
summary(lm.fit_all)

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


