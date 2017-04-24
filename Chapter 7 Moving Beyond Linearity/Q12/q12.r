
p = 100
n = 1000
itr = 5
set.seed(1)
x = matrix(10*rnorm(p*n), ncol=p,nrow=n)
betas = rnorm(p)
beta_0 = rep(0.7,n)
err = rnorm(n)
betas_hat = rnorm(p)

y = beta_0 + x%*%betas + err

err = c()
for(j in 1:itr){
    for(i in 1:p){
        Y = y-x[,-i]%*%betas_hat[-i]
        model = lm(Y~x[,i])
        betas_hat[i] = model$coef[2]
    }
    intercept = model$coef[1]
    pred = intercept + x%*%betas_hat
    err[j]=mean((pred-y)^2)
}

intercept

plot(err,xlab="Iterations", type="l")

betas_hat

betas

intercept


