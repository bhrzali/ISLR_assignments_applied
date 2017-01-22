
library(ISLR)
names(Auto)

mpg_median = median(Auto$mpg)
mpg01 = rep(0,length(Auto$mpg))
mpg01[Auto$mpg>mpg_median]=1
new_auto = data.frame(Auto,mpg01)
names(new_auto)

cor(new_auto[,-9])

pairs(new_auto[,-9])

nrow(new_auto)

new_auto = new_auto[sample(nrow(new_auto)),] #shuffle the data
training_data = new_auto[1:292,]
test_data = new_auto[293:nrow(new_auto),]
#removing the names column
training_data = training_data[,-9]
test_data = test_data[,-9]

library(MASS)
lda.model = lda(mpg01~cylinders+displacement+horsepower+weight+origin,data=training_data)
lda.model

lda.pred = predict(lda.model,newdata=test_data)
#test error percentage
mean(lda.pred$class!=test_data$mpg01)*100

qda.model = qda(mpg01~cylinders+displacement+horsepower+weight+origin,data=training_data)
qda.model

qda.pred = predict(qda.model,newdata=test_data)
#test error percentage
mean(qda.pred$class!=test_data$mpg01)*100

lgs.model = glm(mpg01~cylinders+displacement+horsepower+weight+origin,data=training_data,family=binomial)
summary(lgs.model)

lgs.probs = predict(lgs.model,newdata=test_data,type="response")
lgs.pred = rep(0,nrow(test_data))
lgs.pred[lgs.probs>0.5]=1
#test error percentage
mean(lgs.pred!=test_data$mpg01)*100

library(class)
train.y = training_data$mpg01
test.y = test_data$mpg01
knn.pred = knn(training_data,test_data,train.y,k=1)
#test error rate wtih k =1
mean(knn.pred!=test.y)*100

knn.pred = knn(training_data,test_data,train.y,k=3)
#test error rate wtih k =3
mean(knn.pred!=test.y)*100

knn.pred = knn(training_data,test_data,train.y,k=5)
#test error rate wtih k =5
mean(knn.pred!=test.y)*100

knn.pred = knn(training_data,test_data,train.y,k=10)
#test error rate wtih k =10
mean(knn.pred!=test.y)*100


