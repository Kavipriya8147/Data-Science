A<-crime_data[,-1]
View(A)
########EDA
###First moment of Business solution
mean(A$Murder)
mean(A$Assault)
mean(A$UrbanPop)
mean(A$Rape)
median(A$Murder)
median(A$Assault)
median(A$UrbanPop)
median(A$Rape)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(A$Murder)
getmode(A$Assault)
getmode(A$UrbanPop)
getmode(A$Rape)
###Second moment of Business solution
var(A$Murder)
var(A$Assault)
var(A$UrbanPop)
var(A$Rape)
sd(A$Murder)
sd(A$Assault)
sd(A$UrbanPop)
sd(A$Rape)
range(A$Murder)
range(A$Assault)
range(A$UrbanPop)
range(A$Rape)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(A$Murder)
rangevalue(A$Assault)
rangevalue(A$UrbanPop)
rangevalue(A$Rape)
###Third moment of Business solution
skewness(A$Murder)
skewness(A$Assault)
skewness(A$UrbanPop)
skewness(A$Rape)
###Fourth moment of Business solution
kurtosis(A$Murder)
kurtosis(A$Assault)
kurtosis(A$UrbanPop)
kurtosis(A$Rape)


##Visualization Technique
boxplot(A$Murder)
boxplot(A$Assault)
boxplot(A$UrbanPop)
boxplot(A$Rape)
hist(A$Murder)
hist(A$Assault)
hist(A$UrbanPop)
hist(A$Rape)
barplot(A$Murder)
barplot(A$Assault)
barplot(A$UrbanPop)
barplot(A$Rape)
###Normal Quantile Plot(qqplot)
qqnorm(A$Murder)
qqline(A$Murder)
qqnorm(A$Assault)
qqline(A$Assault)
qqnorm(A$UrbanPop)
qqline(A$UrbanPop)
qqnorm(A$Rape)
qqline(A$Rape)
summary(A)
####Scatter Plot
plot(A$Murder)
plot(A$Assault)
plot(A$UrbanPop)
plot(A$Rape)
####Matrix of Scatterplot
pairs(A)
####Correlation
cor(A)
cor(A$Murder,A$Assault)
cor(A$UrbanPop,A$Rape)
#####Normalisation using Scale function
normaliseddata<-scale(crime_data[,2:4])
summary(normaliseddata)#####data should be from -3.4 to +3.4
####Model Building with euclidean.
d<-dist(normaliseddata,method="euclidean")
fit<-hclust(d,method="complete")
#####Dendrogram
plot(fit)
#####to get all number in same line using hang=-1.
plot(fit,hang=-1)
####Cut tree when k is 4.
groups<-cutree(fit,k=4)
groups
####to get the tree with rectangle shape with red color when k=4
rect.hclust(fit,k=4,border="red")
#####making matrix assining membership as name in groups
membership<-as.matrix(groups)
######membership in A
fin<-data.frame(A,membership)
View(fin)
###### to make membership in first column
final<-fin[,c(ncol(fin),1:(ncol(fin)-1))]
final
####using aggregate making membership as List with function as mean.
aggregate(A,by=list(fin$membership),FUN = mean)



