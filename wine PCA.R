s<-wine[,]
View(s)
#######EDA
####first moment of Business Solution
mean(s$Type)
mean(s$Alcohol)
mean(s$Malic)
mean(s$Ash)
mean(s$Alcalinity)
mean(s$Magnesium)
mean(s$Phenols)
mean(s$Flavanoids)
mean(s$Nonflavanoids)
mean(s$Proanthocyanins)
mean(s$Color)
mean(s$Hue)
mean(s$Dilution)
mean(s$Proline)
median(s$Type)
median(s$Alcohol)
median(s$Malic)
median(s$Ash)
median(s$Alcalinity)
median(s$Magnesium)
median(s$Phenols)
median(s$Flavanoids)
median(s$Nonflavanoids)
median(s$Proanthocyanins)
median(s$Color)
median(s$Hue)
median(s$Dilution)
median(s$Proline)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(s$Type)
getmode(s$Alcohol)
getmode(s$Malic)
getmode(s$Ash)
getmode(s$Alcalinity)
getmode(s$Magnesium)
getmode(s$Phenols)
getmode(s$Flavanoids)
getmode(s$Nonflavanoids)
getmode(s$Proanthocyanins)
getmode(s$Color)
getmode(s$Hue)
getmode(s$Dilution)
getmode(s$Proline)

###Second moment of Business solution
var(s$Type)
var(s$Alcohol)
var(s$Malic)
var(s$Ash)
var(s$Alcalinity)
var(s$Magnesium)
var(s$Phenols)
var(s$Flavanoids)
var(s$Nonflavanoids)
var(s$Proanthocyanins)
var(s$Color)
var(s$Hue)
var(s$Dilution)
var(s$Proline)
sd(s$Type)
sd(s$Alcohol)
sd(s$Malic)
sd(s$Ash)
sd(s$Alcalinity)
sd(s$Magnesium)
sd(s$Phenols)
sd(s$Flavanoids)
sd(s$Nonflavanoids)
sd(s$Proanthocyanins)
sd(s$Color)
sd(s$Hue)
sd(s$Dilution)
sd(s$Proline)
range(s$Type)
range(s$Alcohol)
range(s$Malic)
range(s$Ash)
range(s$Alcalinity)
range(s$Magnesium)
range(s$Phenols)
range(s$Flavanoids)
range(s$Nonflavanoids)
range(s$Proanthocyanins)
range(s$Color)
range(s$Hue)
range(s$Dilution)
range(s$Proline)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(s$Type)
rangevalue(s$Alcohol)
rangevalue(s$Malic)
rangevalue(s$Ash)
rangevalue(s$Alcalinity)
rangevalue(s$Magnesium)
rangevalue(s$Phenols)
rangevalue(s$Flavanoids)
rangevalue(s$Nonflavanoids)
rangevalue(s$Proanthocyanins)
rangevalue(s$Color)
rangevalue(s$Hue)
rangevalue(s$Dilution)
rangevalue(s$Proline)

###Third moment of Business solution
skewness(s$Type)
skewness(s$Alcohol)
skewness(s$Malic)
skewness(s$Ash)
skewness(s$Alcalinity)
skewness(s$Magnesium)
skewness(s$Phenols)
skewness(s$Flavanoids)
skewness(s$Nonflavanoids)
skewness(s$Proanthocyanins)
skewness(s$Color)
skewness(s$Hue)
skewness(s$Dilution)
skewness(s$Proline)

###Fourth moment of Business solution
kurtosis(s$Type)
kurtosis(s$Alcohol)
kurtosis(s$Malic)
kurtosis(s$Ash)
kurtosis(s$Alcalinity)
kurtosis(s$Magnesium)
kurtosis(s$Phenols)
kurtosis(s$Flavanoids)
kurtosis(s$Nonflavanoids)
kurtosis(s$Proanthocyanins)
kurtosis(s$Color)
kurtosis(s$Hue)
kurtosis(s$Dilution)
kurtosis(s$Proline)

##Visualization Technique

boxplot(s$Type)
boxplot(s$Alcohol)
boxplot(s$Malic)
boxplot(s$Ash)
boxplot(s$Alcalinity)
boxplot(s$Magnesium)
boxplot(s$Phenols)
boxplot(s$Flavanoids)
boxplot(s$Nonflavanoids)
boxplot(s$Proanthocyanins)
boxplot(s$Color)
boxplot(s$Hue)
boxplot(s$Dilution)
boxplot(s$Proline)
hist(s$Type)
hist(s$Alcohol)
hist(s$Malic)
hist(s$Ash)
hist(s$Alcalinity)
hist(s$Magnesium)
hist(s$Phenols)
hist(s$Flavanoids)
hist(s$Nonflavanoids)
hist(s$Proanthocyanins)
hist(s$Color)
hist(s$Hue)
hist(s$Dilution)
hist(s$Proline)

barplot(s$Type)
barplot(s$Alcohol)
barplot(s$Malic)
barplot(s$Ash)
barplot(s$Alcalinity)
barplot(s$Magnesium)
barplot(s$Phenols)
barplot(s$Flavanoids)
barplot(s$Nonflavanoids)
barplot(s$Proanthocyanins)
barplot(s$Color)
barplot(s$Hue)
barplot(s$Dilution)
barplot(s$Proline)


###Normal Quantile Plot(qqplot)
qqnorm(s$Type)
qqline(s$Type)
qqnorm(s$Alcohol)
qqline(s$Alcohol)
qqnorm(s$Malic)
qqline(s$Malic)
qqnorm(s$Ash)
qqline(s$Ash)
qqnorm(s$Alcalinity)
qqline(s$Alcalinity)
qqnorm(s$Magnesium)
qqline(s$Magnesium)
qqnorm(s$Phenols)
qqline(s$Phenols)
qqnorm(s$Flavanoids)
qqline(s$Flavanoids)
qqnorm(s$Nonflavanoids)
qqline(s$Nonflavanoids)
qqnorm(s$Proanthocyanins)
qqline(s$Proanthocyanins)
qqnorm(s$Color)
qqline(s$Color)
qqnorm(s$Hue)
qqline(s$Hue)
qqnorm(s$Dilution)
qqline(s$Dilution)
qqnorm(s$Proline)
qqline(s$Proline)
###Scatter Plot
plot(s)
###Matrix of Scatter Plot
pairs(s)
####Correlation
cor(s$Type,s$Alcohol)
cor(s$Malic,s$Ash)
cor(s$Alcalinity,s$Magnesium)
cor(s$Phenols,s$Flavanoids)
cor(s$Nonflavanoids,s$Proanthocyanins)
cor(s$Color,s$Hue)
cor(s$Dilution,s$Proline)
cor(s)
###Model

pca<-princomp(s,cor=TRUE,scores=TRUE,covmat=NULL)
summary(pca)
str(pca)
loadings(pca)
plot(pca)#####graph showing importance of Principalcomponents

###Pca$loadings ####top 3 pca scores which will represent the entire data
pca$scores[,1:3]

####Top 3 Pc scores and bind them with s
mydata<-cbind(s,pca$scores[,1:3])
View(mydata)
#####Preparing the Data for Further Clustering
Clus<-mydata[,8:10]
View(Clus)
####Normalizing the data
b<-scale(s)
di<-dist(b,method = "euclidean")####using Euclidean Distance We can also use Manhattan Distance
di<-dist(b,method = "manhattan")
#####Clustering the data using Hclust 
fit<-hclust(di,method = "complete")
plot(fit)###Dendrogram
plot(fit,hang = -1)#####Dendrogram with hang=-1
groups<-cutree(fit,6)####Cutting the tree with 6 clusters
rect.hclust(fit,k=6,border="blue")#####Marking blue co;our with clusters
m<-as.matrix(groups)####m variable as seperate
View(m)
#####Combine the m variable with 's' dataset
final<-data.frame(s,m)
View(final)
#####to make m variable in 1st column
f1<-final[,c(ncol(final),1:(ncol(final-1)))]
View(f1)
 
aggregate((s[,]),by=list(final$m),FUN=mean)


#####Clustering the data using Kmeansclustering
fit1<-kmeans(b,4)
str(fit1)
####using Animation to see the clusters
km<-kmeans.ani(b,8)
str(km)
####to get the  cluster assinging
fit1$cluster
Fl<-data.frame(s,fit1$cluster)
Fl
finl1<-Fl[,c(ncol(Fl),1:(ncol(fin)-1))]
finl1
aggregate(s,by=list(fit1$cluster), FUN = mean)
#####Elbow Curve & Squrt(n/2) to decide the k value

wss<-(nrow(b)-1)*sum(apply(b),4,var)
twss()
for(i in 1:12)wss[i]=sum(kmeans(b,centers =i)$Withiness)
plot(1:8,wss,tybe="b",xlab = "Number of clusters",ylab="within groups sum of Squares")
title(sub="K Means Clustering Scree Plot")


