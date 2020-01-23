D<-`50_Startups`[,-4]
View(D)
####EDA
###First moment of Business solution
mean(D$R.D.Spend)
mean(D$Administration)
mean(D$Marketing.Spend)
mean(D$Profit)
median(D$R.D.Spend)
median(D$Administration)
median(D$Marketing.Spend)
median(D$Profit)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(D$R.D.Spend)
getmode(D$Administration)
getmode(D$Marketing.Spend)
getmode(D$Profit)
###Second moment of Business solution
var(D$R.D.Spend)
var(D$Administration)
var(D$Marketing.Spend)
var(D$Profit)
sd(D$R.D.Spend)
sd(D$Administration)
sd(D$Marketing.Spend)
sd(D$Profit)
range(D$R.D.Spend)
range(D$Administration)
range(D$Marketing.Spend)
range(D$Profit)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(D$R.D.Spend)
rangevalue(D$Administration)
rangevalue(D$Marketing.Spend)
rangevalue(D$Profit)
###Third moment of Business solution
skewness(D$R.D.Spend)
skewness(D$Administration)
skewness(D$Marketing.Spend)
skewness(D$Profit)
###Fourth moment of Business solution
kurtosis(D$R.D.Spend)
kurtosis(D$Administration)
kurtosis(D$Marketing.Spend)
kurtosis(D$Profit)
##Visualization Technique
boxplot(D$R.D.Spend)
boxplot(D$Administration)
boxplot(D$Marketing.Spend)
boxplot(D$Profit)
hist(D$R.D.Spend)
hist(D$Administration)
hist(D$Marketing.Spend)
hist(D$Profit)
barplot(D$R.D.Spend)
barplot(D$Administration)
barplot(D$Marketing.Spend)
barplot(D$Profit)
###Normal Quantile Plot(qqplot)
qqnorm(D$R.D.Spend)
qqline(D$R.D.Spend)
qqnorm(D$Administration)
qqline(D$Administration)
qqnorm(D$Marketing.Spend)
qqline(D$Marketing.Spend)
qqnorm(D$Profit)
qqline(D$Profit)
###Scatter Plot to check Linearity of my data
plot(D$Profit,D$R.D.Spend) #####Positive
plot(D$Profit,D$Administration)####   no
plot(D$Profit,D$Marketing.Spend) ####positive with many outliers

###Matrix of Scatter Plot
pairs(D)
####Correlation
cor(D$R.D.Spend,D$Profit)
cor(D$R.D.Spend,D$Marketing.Spend)
cor(D$R.D.Spend,D$Administration)
cor(D$Administration,D$Marketing.Spend)
cor(D$Administration,D$Profit)
cor(D$Marketing.Spend,D$Profit)
cor(D)
summary(D)


######Model Building
model<-lm(D$Profit~.,data = D)

summary(model)
model_1<-lm(D$Profit~D$Administration)
summary(model_1)
model_2<-lm(D$Profit~D$Marketing.Spend)
summary(model_2)
model_3<-lm(D$Profit~D$Administration+D$Marketing.Spend)
summary(model_3)

#####To get full correlation with y and all x we r using Partial Correlation.
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(D))
####Diagnostic Plots
###Residual Plot,QQ pLot,Std.Residuals.
plot(model)



