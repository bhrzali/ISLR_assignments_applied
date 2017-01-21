
library(ISLR)

summary(Weekly)

pairs(Weekly)

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly, family=binomial)
summary(glm.fit)

glm.probs = predict(glm.fit,newdata=Weekly,type="response")
glm.pred = rep("Down",length(Weekly$Direction))
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Weekly$Direction)

#fraction of correct prediction
mean(glm.pred==Weekly$Direction)*100

#Up prediction accuracy
557/(430+557)*100

#Down Prediction accuracy
54/(54+48)*100

training = (Weekly$Year<2009)
training_data = Weekly[training,]
test_data = Weekly[!training,]
glm.fit2 = glm(Direction~Lag2,data=training_data,family=binomial)
summary(glm.fit2)

glm.probs2 = predict(glm.fit2,newdata=test_data,type="response")
glm.pred2 = rep("Down",length(test_data$Direction))
glm.pred2[glm.probs2>0.5]="Up"
table(glm.pred2,test_data$Direction)

#Overall prediction accuracy
mean(glm.pred2==test_data$Direction)*100

#Up prediction accuracy
56/(56+34)*100

#Down prediction accuracy
9/(9+5)*100

#LDA
library(MASS)
lda.fit = lda(Direction~Lag2,data=training_data)
lda.fit

lda.pred = predict(lda.fit,newdata=test_data)
table(lda.pred$class,test_data$Direction)

#Overall prediction accuracy
mean(lda.pred$class==test_data$Direction)*100

#Up prediction accuracy
56/(34+56)*100

#Down prediction accuracy
9/(9+5)*100

#QDA
qda.fit = qda(Direction~Lag2,data=training_data)
qda.fit

qda.pred = predict(qda.fit, newdata=test_data)
table(qda.pred$class,test_data$Direction)

#Overall prediction accuracy
mean(qda.pred$class==test_data$Direction)*100

#Up prediction accuracy
61/(61+43)*100

library(class)
knn_train = matrix(training_data$Lag2)
knn_train_y = training_data$Direction
knn_test = matrix(test_data$Lag2)
knn_test_y = test_data$Direction
set.seed(1)
knn.pred = knn(knn_train,knn_test,knn_train_y,prob=FALSE,k=1)
table(knn.pred,knn_test_y)

#Overall prediction accuracy
mean(knn.pred==knn_test_y)*100

#Up prediction accuracy
31/(31+22)*100

#Down prediction accuracy
21/(21+30)*100

lda.fit2 = lda(Direction~Lag2+I(Lag2^2),data=training_data)
lda.fit2

lda.pred2 = predict(lda.fit2,newdata=test_data)
mean(lda.pred2$class==test_data$Direction)*100

knn_train = scale(training_data[,-ncol(training_data)])
knn_train_y = training_data$Direction
knn_test = scale(test_data[,-ncol(test_data)])
knn_test_y = test_data$Direction

library(class)
knn.pred = knn(knn_train,knn_test,knn_train_y,k=3)

#Prediction accuracy
mean(knn.pred==knn_test_y)*100

table(knn.pred,knn_test_y)

#Up prediction accuracy
55/(55+7)*100

#Down prediction accuracy
36/(36+6)*100


