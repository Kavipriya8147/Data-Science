F<-Fraud_check[,]
####EDA
mean(F$Taxable.Income)
mean(F$City.Population)
mean(F$Work.Experience)
median(F$Taxable.Income)
median(F$City.Population)
median(F$Work.Experience)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(F$Undergrad)
getmode(F$Marital.Status)
getmode(F$Taxable.Income)
getmode(F$City.Population)
getmode(F$Work.Experience)
getmode(F$Urban)
###Second moment of Business solution
var(F$Taxable.Income)
var(F$Work.Experience)
var(F$City.Population)
sd(F$Taxable.Income)
sd(F$City.Population)
sd(F$Work.Experience)
range(F$Taxable.Income)
range(F$City.Population)
range(F$Work.Experience)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(F$Taxable.Income)
rangevalue(F$City.Population)
rangevalue(F$Work.Experience)
###Third moment of Business solution
skewness(F$Taxable.Income)
skewness(F$City.Population)
skewness(F$Work.Experience)
###Fourth moment of Business solution
kurtosis(F$Taxable.Income)
kurtosis(F$City.Population)
kurtosis(F$Work.Experience)
##Visualization Technique
boxplot(F$Taxable.Income)
boxplot(F$City.Population)
boxplot(F$Work.Experience)
str(F)
hist(F$Taxable.Income)
hist(F$City.Population)
hist(F$Work.Experience)
barplot(F$Taxable.Income)
barplot(F$City.Population)
barplot(F$Work.Experience)
###Normal Quantile Plot(qqplot)
qqnorm(F$Taxable.Income)
qqline(F$Taxable.Income)
qqnorm(F$City.Population)
qqline(F$City.Population)
qqnorm(F$Work.Experience)
qqline(F$Work.Experience)
###Scatter Plot
plot(F)
###Matrix of Scatter Plot
pairs(F)
####Correlation
cor(F$Taxable.Income,F$City.Population)
cor(F$Work.Experience,F$Taxable.Income)
cor(F$City.Population,F$Work.Experience)
####DT using C5.0 Package
F$Taxable.Income<-as.factor(F$Taxable.Income)
model=C5.0(F$Taxable.Income~.,data=F)
summary(model)
pred=predict.C5.0(model,F[,-3])
table(F$Taxable.Income,pred)
#####Bagging technique
acc=c()
for (i in 1:10){
  splitratio=createDataPartition(F$Taxable.Income,p=.085,list = FALSE)
train=F[splitratio,]
test=F[-splitratio,]  

fittree=C5.0(train$Taxable.Income~.,data=train)
pred1=predict.C5.0(fittree,test[,-3])
a=table(test$Taxable.Income,pred1)

acc=c(acc,sum(diag(a)/sum(a)))
}
summary(acc)

######Boosting technique
model=C5.0(F$Taxable.Income~.,data=F,trials=10)
summary(model)
pred=predict.C5.0(model,F[,-3])
table(F$Taxable.Income,pred)







#####using dtree
F_tree<-dtree(Taxable.Income~.,data = F)
summary(F_tree)
######Splitting the data with F$Taxable.Income
unrisk<-F[F$Taxable.Income<="30000",]
risk<-F[F$Taxable.Income>="30000",]
F_train<-rbind(unrisk[1:200,],risk[1:200,])
View(F_train)
F_test<-rbind(unrisk[201:600,],risk[201:600,])
View(F_test)
# Building model on training data 
FC5.0_train<-C5.0(F_train[,-10],as.factor(F_train$Taxable.Income))
windows()
plot(FC5.0_train) # Tree graph
# Training accuracy
pred_train <- predict(FC5.0_train,F_train)
pred_train<-as.data.frame(pred_train)
mean(F_train$Taxable.Income==pred_train) # to find % of Accuracy
confusionMatrix(pred_train,F_train$Taxable.Income)


