
library(MASS)
names(Boston)

crime_class = rep(0,length(Boston$crim))
crim_median = median(Boston$crim)
crime_class[Boston$crim>crim_median]=1
new_Boston = data.frame(Boston,crime_class)
names(new_Boston)

cor(new_Boston)

#creating training data and test data
nrow(new_Boston)

#new_Boston = new_Boston[sample(nrow(new_Boston)),]
training_data = new_Boston[1:406,]
test_data = new_Boston[407:nrow(new_Boston),]

lda.model = lda(crime_class~indus+nox+age+dis+rad+tax,data=training_data)
lda.pred = predict(lda.model,newdata=test_data)
table(lda.pred$class,test_data$crime_class)

#error rate
mean(lda.pred$class!=test_data$crime_class)*100

#False positives is high. Increasing the cutoff probability
lda.pred2 = rep(0,length(test_data$crime_class))
lda.pred2[lda.pred$posterior[,2]>0.7]=1
table(lda.pred2,test_data$crime_class)

#error rate
mean(lda.pred2!=test_data$crime_class)*100

lgs.model = glm(crime_class~indus+nox+rad+age+dis+tax,data=training_data,family=binomial)
summary(lgs.model)

lgs.probs = predict(lgs.model,newdata=test_data,type="response")
lgs.pred = rep(0,length(test_data$crime_class))
lgs.pred[lgs.probs>0.5]=1
#error rate
mean(lgs.pred!=test_data$crime_class)*100

table(lgs.pred,test_data$crime_class)

library(class)
train.y = training_data$crime_class
test.y = test_data$crime_class
training_data_k = training_data[,-c(1,2,4,6)]
test_data_k = test_data[,-c(1,2,4,6)]
knn.pred = knn(training_data_k,test_data_k,train.y,k=3)

#error rate
mean(knn.pred!=test.y)*100

table(knn.pred,test.y)
