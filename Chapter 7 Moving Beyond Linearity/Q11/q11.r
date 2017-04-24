
set.seed(1)
x1 = rnorm(100)
x2 = rnorm(100)
err = rnorm(100)

b0 = 3
b1 = 5
b2 = 8
y = b0 + b1*x1 + b2*x2 + err 

b1_hat = 6
m = y-b1_hat*x1
b2_hat = lm(m~x2)$coef[2]
b2_hat

m=y-b2_hat*x2
b1_hat = lm(m~x1)$coef[2]
b1_hat

b1_hat = 6
b0_hat_lst = c()
b1_hat_lst = c()
b2_hat_lst = c()

for(i in 1:1000){
    m1 = y-b1_hat*x1
    model = lm(m1~x2)
    b2_hat = model$coef[2]
    
    m2 = y-b2_hat*x2
    model = lm(m2~x1)
    b1_hat = model$coef[2]
    
    b0_hat_lst[i]=model$coef[1]
    b1_hat_lst[i]=b1_hat
    b2_hat_lst[i]=b2_hat
}

plot(NULL,NULL,xlab="Iteration",ylab="coef",xlim=c(1,1000),ylim=c(1,10))
lines(b0_hat_lst,col="red")
lines(b1_hat_lst,col="blue")
lines(b2_hat_lst,col="green")
legend(0,10,legend=c("b0_hat","b1_hat","b2_hat"),col=c("red","blue","green"),lty=1, cex=0.8)

lm.model = lm(y~x1+x2)
summary(lm.model)

plot(NULL,NULL,xlab="Iteration",ylab="coef",xlim=c(1,1000),ylim=c(1,10))
lines(b0_hat_lst,col="red")
lines(b1_hat_lst,col="blue")
lines(b2_hat_lst,col="green")
abline(lm.model$coef[1],0,col="red",lty=2)
abline(lm.model$coef[2],0,col="blue",lty=2)
abline(lm.model$coef[3],0,col="green",lty=2)
legend(0,10,legend=c("b0_hat","b1_hat","b2_hat"),col=c("red","blue","green"),lty=1, cex=0.8)

b1_hat = 6
b0_hat_lst = c()
b1_hat_lst = c()
b2_hat_lst = c()

for(i in 1:2){
    m1 = y-b1_hat*x1
    model = lm(m1~x2)
    b2_hat = model$coef[2]
    
    m2 = y-b2_hat*x2
    model = lm(m2~x1)
    b1_hat = model$coef[2]
    
    b0_hat_lst[i]=model$coef[1]
    b1_hat_lst[i]=b1_hat
    b2_hat_lst[i]=b2_hat
}

b0_hat_lst[2]

b1_hat_lst[2]

b2_hat_lst[2]


