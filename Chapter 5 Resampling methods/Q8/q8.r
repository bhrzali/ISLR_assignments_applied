
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x-2*x^2+rnorm(100)

plot(x,y)

set.seed(1)
library(boot)
glm.model = glm(y~x,data=data.frame(x,y))
cv.err = cv.glm(data.frame(x,y),glm.model)
cv.err$delta

glm.model = glm(y~x+I(x^2),data=data.frame(x,y))
cv.err = cv.glm(data.frame(x,y),glm.model)
cv.err$delta

glm.model = glm(y~x+I(x^2)+I(x^3),data=data.frame(x,y))
cv.err = cv.glm(data.frame(x,y),glm.model)
cv.err$delta

glm.model = glm(y~x+I(x^2)+I(x^3)+I(x^4),data=data.frame(x,y))
cv.err = cv.glm(data.frame(x,y),glm.model)
cv.err$delta

set.seed(2)
for(i in 1:4){
    glm.model = glm(y~poly(x,i),data=data.frame(x,y))
    cv.err = cv.glm(data.frame(x,y),glm.model)
    print(i)
    print(cv.err$delta)
}

summary(glm.model)
