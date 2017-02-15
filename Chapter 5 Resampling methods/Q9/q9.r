
library(MASS)
summary(Boston)

mean(Boston$medv)

sd(Boston$medv)/sqrt(length(Boston$medv))

library(boot)
set.seed(2)
mean.function = function(data,index){
    d = data$medv
    return(mean(d[index]))
}
bootstrap = boot(data=Boston,statistic=mean.function,R=1000)
bootstrap

#95% confidence interval
print("lower bound")
22.533-2*0.419
print("upper bound")
22.533+2*0.419

t.test(Boston$medv)

median(Boston$medv)

median.function = function(data,index){
    d = data$medv
    return(median(d[index]))
}
boot(data=Boston,statistic = median.function,R=1000)

quantile(Boston$medv, 0.1)

quantile.function = function(data,index){
    d = data$medv
    return(quantile(d[index],0.1))
}
boot(data=Boston,statistic=quantile.function,R=1000)
