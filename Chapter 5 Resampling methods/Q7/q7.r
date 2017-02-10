
library(ISLR)
names(Weekly)

glm.model = glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
summary(glm.model)

cv.glm.model = glm(Direction~Lag1+Lag2,data=Weekly,family=binomial,subset=-1)

class = 'Down'
result=predict(cv.glm.model,newdata=Weekly[1,],type="response")
if(result>0.5){
    class = 'Up'
}
class == Weekly$Direction[1]

err=0
for(i in 1:nrow(Weekly)){
    glm.model = glm(Direction~Lag1+Lag2,data=Weekly,family=binomial,subset=-i)
    class = 'Down'
    prob = predict(glm.model,newdata=Weekly[i,],type="response")
    if(prob>0.5){
        class = 'Up'
    }
    if(class != Weekly$Direction[i]) err=err+1
}
err

#test error rate estimate
err/nrow(Weekly)*100
