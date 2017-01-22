
library(MASS)

summary(Boston)

Boston2 = Boston
coef_simple = c()
Boston2$chas = as.factor(Boston$chas)
for (p in 2:length(Boston2)){
    pred = Boston[,p]
    print(colnames(Boston2)[p])
    lm.model = lm(crim~pred,data=Boston)
    coef_simple = c(coef_simple, coef(lm.model)[-1]) # for part (c)
    print(summary(lm.model))
}

summary(Boston2)

lm.model_all = lm(crim~.,data=Boston2)
summary(lm.model_all)

coef_multi = coef(lm.model_all)[-1]
plot(coef_simple, coef_multi)

for (p in 2:length(Boston2)){
    pred = Boston2[,p]
    if (colnames(Boston)[p]!="chas"){
        print(colnames(Boston)[p])
        lm.model = lm(crim~poly(pred,3), data=Boston)
        print(summary(lm.model))
    }
}


