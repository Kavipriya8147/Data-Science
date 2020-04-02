F<-forestfires[,c(-1,-2,-31)]
View(F)
####EDA
###First moment of Business solution
mean(F$FFMC)
mean(F$DMC)
mean(F$DC)
mean(F$ISI)
mean(F$temp)
mean(F$RH)
mean(F$wind)
mean(F$rain)
mean(F$area)
mean(F$dayfri)
mean(F$daymon)
mean(F$daysat)
mean(F$daysun)
mean(F$daythu)
mean(F$daytue)
mean(F$daywed)
mean(F$monthapr)
mean(F$monthaug)
mean(F$monthdec)
mean(F$monthfeb)
mean(F$monthjan)
mean(F$monthjul)
mean(F$monthjun)
mean(F$monthmar)
mean(F$monthmay)
mean(F$monthnov)
mean(F$monthoct)
mean(F$monthsep)
median(F$FFMC)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(F$FFMC)
###Second moment of Business solution
var(F$FFMC)
sd(F$FFMC)
range(F$FFMC)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(F$FFMC)
###Third moment of Business solution
skewness(F$FFMC)
###Fourth moment of Business solution
kurtosis(F$FFMC)

##Visualization Technique
boxplot(F$FFMC)
hist(F$FFMC)
barplot(F$FFMC)
####Normal Quantile
qqnorm(F$FFMC)
qqline(F$FFMC)
attach(F)
median(F$DMC)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(DMC)
###Second moment of Business solution
var(DMC)
sd(DMC)
range(DMC)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(DMC)
###Third moment of Business solution
skewness(DMC)
###Fourth moment of Business solution
kurtosis(DMC)

##Visualization Technique
boxplot(DMC)
hist(DMC)
barplot(DMC)
####Normal Quantile
qqnorm(DMC)
qqline(DMC)

median(F$DC)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(DC)
###Second moment of Business solution
var(DC)
sd(DC)
range(DC)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(DC)
###Third moment of Business solution
skewness(DC)
###Fourth moment of Business solution
kurtosis(DC)

##Visualization Technique
boxplot(DC)

hist(DC)
barplot(DC)
####Normal Quantile
qqnorm(DC)
qqline(DC)

median(F$ISI)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(ISI)
###Second moment of Business solution
var(ISI)
sd(ISI)
range(ISI)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(ISI)
###Third moment of Business solution
skewness(ISI)
###Fourth moment of Business solution
kurtosis(ISI)

##Visualization Technique
boxplot(ISI)
hist(ISI)
barplot(ISI)
####Normal Quantile
qqnorm(ISI)
qqline(ISI)
attach(F)
median(F$temp)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(temp)
###Second moment of Business solution
var(temp)
sd(temp)
range(temp)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(temp)
###Third moment of Business solution
skewness(temp)
###Fourth moment of Business solution
kurtosis(temp)

##Visualization Technique
boxplot(temp)
hist(temp)
barplot(temp)
####Normal Quantile
qqnorm(temp)
qqline(temp)

median(F$RH)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(RH)
###Second moment of Business solution
var(RH)
sd(RH)
range(RH)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(RH)
###Third moment of Business solution
skewness(RH)
###Fourth moment of Business solution
kurtosis(RH)

##Visualization Technique
boxplot(RH)
hist(RH)
barplot(RH)
####Normal Quantile
qqnorm(RH)
qqline(RH)

median(F$wind)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(wind)
###Second moment of Business solution
var(wind)
sd(wind)
range(wind)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(wind)
###Third moment of Business solution
skewness(wind)
###Fourth moment of Business solution
kurtosis(wind)

##Visualization Technique
boxplot(wind)
hist(wind)
barplot(wind)
####Normal Quantile
qqnorm(wind)
qqline(wind)

median(F$rain)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(rain)
###Second moment of Business solution
var(rain)
sd(rain)
range(rain)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(rain)
###Third moment of Business solution
skewness(rain)
###Fourth moment of Business solution
kurtosis(rain)

##Visualization Technique
boxplot(rain)
hist(rain)
barplot(rain)
####Normal Quantile
qqnorm(rain)
qqline(rain)

median(F$area)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(area)
###Second moment of Business solution
var(area)
sd(area)
range(area)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(area)
###Third moment of Business solution
skewness(area)
###Fourth moment of Business solution
kurtosis(area)

##Visualization Technique
boxplot(area)
hist(area)
barplot(area)
####Normal Quantile
qqnorm(area)
qqline(area)

median(F$dayfri)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(dayfri)
###Second moment of Business solution
var(dayfri)
sd(dayfri)
range(dayfri)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(dayfri)
###Third moment of Business solution
skewness(dayfri)
###Fourth moment of Business solution
kurtosis(dayfri)

##Visualization Technique
boxplot(dayfri)
hist(dayfri)
barplot(dayfri)
####Normal Quantile
qqnorm(dayfri)
qqline(dayfri)

median(F$daymon)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(daymon)
###Second moment of Business solution
var(daymon)
sd(daymon)
range(daymon)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(daymon)
###Third moment of Business solution
skewness(daymon)
###Fourth moment of Business solution
kurtosis(daymon)

##Visualization Technique
boxplot(daymon)
hist(daymon)
barplot(daymon)
####Normal Quantile
qqnorm(daymon)
qqline(daymon)


#####Normalization Function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

F_nor<-as.data.frame(lapply(F[,],normalize))
View(F_nor)
####Create training and test data
F_train<-F_nor[1:259,]
F_test<-F_nor[260:517,]
####Training the neuralnet
m<-neuralnet(formula= F_train$area~.,data=F_train)
###visualize the NN
plot(m)
###Evaluating the performance of the model
re<-compute(m,F_test)
###to get the predicted Values
str(re)
Stren_predt<-re$net.result
View(Stren_predt)
re$neurons
cor(Stren_predt,F_test$area)
plot(re$net.result,F_test$area)

####hidden layer Adding with function "hidden"
m2<-neuralnet(formula= F_train$area~.,data=F_train,hidden = 20)###adding 20 layer
plot(m2)
###Evaluating the performance of the model
re2<-compute(m2,F_test)
###to get the predicted Values
str(re2)
Stren_predt2<-re2$net.result
View(Stren_predt2)
re2$neurons
cor(Stren_predt2,F_test$area)
plot(re2$net.result,F_test$area)
