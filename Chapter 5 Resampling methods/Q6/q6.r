
library(ISLR)

set.seed(1)
glm.model = glm(default~income+balance,data=Default,family=binomial)
summary(glm.model)

set.seed(1)
boot.fn = function(data, index){
    glm.model = glm(default~income+balance,data=data,family=binomial,subset=index)
    return(coef(glm.model))
}

library(boot)
boot(data=Default, statistic=boot.fn, R=50)
