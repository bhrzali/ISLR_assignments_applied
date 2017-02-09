
library(ISLR)
glm.model = glm(default~income+balance,data=Default,family=binomial)
glm.model

set.seed(1)
train = sample(nrow(Default),8000)

glm.model = glm(default~income+balance, data=Default, family=binomial, subset=train)

glm.prob = predict(glm.model,newdata=Default[-train,], type="response") 
glm.pred = rep("No",length(glm.prob))
glm.pred[glm.prob>0.5]="Yes"
#computing error rate
sum(glm.pred!=Default[-train,]$default)/length(glm.pred)*100

for(i in 2:4){
    set.seed(i)
    train = sample(nrow(Default),8000)
    glm.model = glm(default~income+balance,data=Default,family=binomial, subset=train)
    glm.prob = predict(glm.model,newdata=Default[-train,],type="response")
    glm.pred = rep("No",length(glm.prob))
    glm.pred[glm.prob>0.5]="Yes"
    print(sum(glm.pred!=Default[-train,]$default)/length(glm.pred)*100)
}

(2.15+2.55+2.85)/3

set.seed(1)
train = sample(nrow(Default),8000)
glm.model = glm(default~income+balance+student,data=Default,family=binomial,subset=train)
glm.prob = predict(glm.model,newdata=Default[-train,],type="response")
glm.pred = rep("No",length(glm.prob))
glm.pred[glm.prob>0.5]="Yes"
#computing the error rate
sum(glm.pred!=Default[-train,]$default)/length(glm.pred)*100


