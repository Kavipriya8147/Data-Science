####load the dataset with name "M"
M<-sms_raw_NB[,]
View(M)####to view the dataset
M$type<-factor(M$type)
####to get structure of the dataset for type column
str(M$type)
####using table function implementing table 
table(M$type)####ham is 4812 , spam is 747
summary(M)
####creating barplot with 2 variables
barplot(table(as.factor(M[,1]),as.factor(M[,2])),legend=c("ham","spam"))
plot(as.factor(M[M$text=="ham",2]))
plot(as.factor(M[M$text=="spam",2]))
str(M)
####Creating train and test data
train<-M[1:3000,]
test<-M[3001:5559,]
library(e1071)
mod<-naiveBayes(train$text~.,data=train[,-1])
pred<-predict(mod,newdata = test[,-1])
View(pred)
mean(pred)
mean(pred==test[,])
mean
acc<-c(acc,mean(pred==test[,]))
acc
