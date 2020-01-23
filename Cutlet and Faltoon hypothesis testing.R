cutlet<-Cutlets[,]
View(cutlet)
####EDA
###First moment of Business solution
mean(cutlet$Unit.A)
mean(cutlet$Unit.B)
median(cutlet$Unit.A)
median(cutlet$Unit.B)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(cutlet$Unit.A)
getmode(cutlet$Unit.B)
###Second moment of Business solution
var(cutlet$Unit.A)
var(cutlet$Unit.B)
sd(cutlet$Unit.A)
sd(cutlet$Unit.B)
range(cutlet$Unit.A)
range(cutlet$Unit.B)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(cutlet$Unit.A)
rangevalue(cutlet$Unit.B)
###Third moment of Business solution
skewness(cutlet$Unit.A)
skewness(cutlet$Unit.B)
###Fourth moment of Business solution
kurtosis(cutlet$Unit.A)
kurtosis(cutlet$Unit.B)
##Visualization Technique
boxplot(cutlet$Unit.A)
boxplot(cutlet$Unit.B)
boxplot(cutlet$Unit.A)$out
boxplot(cutlet$Unit.B)$out
b=boxplot(cutlet$Unit.A,horizontal = TRUE)
a=boxplot(cutlet$Unit.B,horizontal = TRUE)
hist(cutlet$Unit.A)
hist(cutlet$Unit.B)
barplot(cutlet$Unit.A)
barplot(cutlet$Unit.B)
str(cutlet)
summary(cutlet)
###Normal Quantile Plot(qqplot)
qqnorm(cutlet$Unit.A)
qqline(cutlet$Unit.A)
qqnorm(cutlet$Unit.B)
qqline(cutlet$Unit.B)
scale(cutlet$Unit.A)
scale(cutlet$Unit.B)
##Normalization Test toncheck the data is normally distributed or not
#Ho:Data is normally Distributed.
#Ha:Data is not normally Distributed.
shapiro.test(cutlet$Unit.A)
shapiro.test(cutlet$Unit.B)
##p value is high Ho fly -->so accept Ho
 t.test(cutlet$Unit.A,cutlet$Unit.B,alternative="two.sided",conf.level = .95,correct=TRUE)
 
 
 
k<-Faltoons[,]
View(k) 
####EDA
###First moment of Business solution
mean and median cannot be done since its a categorical data. 

getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(k$Weekdays)
getmode(k$Weekend)

attach(k)
#####Grouping into one column
stacked<-stack(k$Weekend)
View(stacked)
####using table function for creating Frequency table.
table(k$Weekdays,k$Weekend)

t2 <- prop.table(table(k$weekdays))
t1 <- table(k$Weekend)
chisq
chisq.test(table(k$Weekdays,k$Weekend))
# p-value = 1> 0.05  => Accept null hypothesis
# => so,% of male vs female walking into the store differs based on the day of the week.