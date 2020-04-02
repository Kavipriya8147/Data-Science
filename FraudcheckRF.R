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
####table of taxable income
table(F$Taxable.Income)
# table or proportation of enteries in the datasets. 
round(prop.table(table(F$Taxable.Income))*100,1)
summary(F[c("Work.Experience")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
F_n <- as.data.frame(lapply(F[,c(-1,-2,-6)], norm))
View(F_n)
F_n["Taxable.Income"] <- F$Taxable.Income
  # Building a random forest model on training data 
F_forest <- randomForest(Taxable.Income~.,data=F_n,importance=TRUE)
plot(F_forest)
####Accuracy
acc_F <- mean(F_n$Taxable.Income==predict(F_forest))
acc_F
###Variable Importance PLOT
varImpPlot(F_forest)

