G<-`glass.(2)`[,]
View(G)
####EDA
###First moment of Business solution
mean(G$RI)
mean(G$Na)
mean(G$Mg)
mean(G$Al)
mean(G$Si)
mean(G$K)
mean(G$Ca)
mean(G$Ba)
mean(G$Fe)
median(G$RI)
median(G$Na)
median(G$Mg)
median(G$Al)
median(G$Si)
median(G$K)
median(G$Ca)
median(G$Ba)
median(G$Fe)
median(G$Type)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(G$RI)
getmode(G$Na)
getmode(G$Mg)
getmode(G$Al)
getmode(G$Si)
getmode(G$K)
getmode(G$Ca)
getmode(G$Ba)
getmode(G$Fe)
###Second moment of Business solution
var(G$RI)
var(G$Na)
var(G$Mg)
var(G$Al)
var(G$Si)
var(G$K)
var(G$Ca)
var(G$Ba)
var(G$Fe)
sd(G$RI)
sd(G$Na)
sd(G$Mg)
sd(G$Al)
sd(G$Si)
sd(G$K)
sd(G$Ca)
sd(G$Ba)
sd(G$Fe)
range(G$RI)
range(G$Na)
range(G$Mg)
range(G$Al)
range(G$Si)
range(G$K)
range(G$Ca)
range(G$Ba)
range(G$Fe)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(G$RI)
rangevalue(G$Na)
rangevalue(G$Mg)
rangevalue(G$Al)
rangevalue(G$Si)
rangevalue(G$K)
rangevalue(G$Ca)
rangevalue(G$Ba)
rangevalue(G$Fe)
###Third moment of Business solution
skewness(G$RI)
skewness(G$Na)
skewness(G$Mg)
skewness(G$Al)
skewness(G$Si)
skewness(G$K)
skewness(G$Ca)
skewness(G$Ba)
skewness(G$Fe)
###Fourth moment of Business solution
kurtosis(G$RI)
kurtosis(G$Na)
kurtosis(G$Mg)
kurtosis(G$Al)
kurtosis(G$Si)
kurtosis(G$K)
kurtosis(G$Ca)
kurtosis(G$Ba)
kurtosis(G$Fe)
##Visualization Technique
boxplot(G$RI)
boxplot(G$Na)
boxplot(G$Mg)
boxplot(G$Al)
boxplot(G$Si)
boxplot(G$K)
boxplot(G$Ca)
boxplot(G$Ba)
boxplot(G$Fe)
hist(G$RI)
hist(G$Na)
hist(G$Mg)
hist(G$Al)
hist(G$Si)
hist(G$K)
hist(G$Ca)
hist(G$Ba)
hist(G$Fe)
barplot(G$RI)
barplot(G$Na)
barplot(G$Mg)
barplot(G$Al)
barplot(G$Si)
barplot(G$K)
barplot(G$Ca)
barplot(G$Ba)
barplot(G$Fe)
###Normal Quantile Plot(qqplot)
qqnorm(G$RI)
qqline(G$RI)
qqnorm(G$Na)
qqline(G$Na)
qqnorm(G$Mg)
qqline(G$Mg)
qqnorm(G$Al)
qqline(G$Al)
qqnorm(G$Si)
qqline(G$Si)
qqnorm(G$K)
qqline(G$K)
qqnorm(G$Ca)
qqline(G$Ca)
qqnorm(G$Ba)
qqline(G$Ba)
qqnorm(G$Fe)
qqline(G$Fe)

###Scatter Plot
plot(G)
###Matrix of Scatter Plot
pairs(G)
####Correlation
cor(G)
#####Table
table(G$Type)

G$Type<-factor(G$Type,levels=c("1","2","3","4","5","6","7"),labels = c("building_windows_float_processed","building_windows_non_float_processed","vehicle_windows_float_processed","vehicle_windows_non_float_processed ","containers","tableware","headlamps"))
round(prop.table(table(G$Type)*100,1))
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
G_n <- as.data.frame(lapply(G[1:9], norm))
summary(G$Type)

#create training and test datasets
G_train <- G_n[1:180,]
G_test <- G_n[181:214,]

#Get labels for training and test datasets

G_train_labels <- G[1:180,1]

G_test_labels <- G[181:214,1]

# Build a KNN model on taining dataset
library("class")
G_n_pred<-knn(train = G_train,test = G_test,cl=G_train_labels,k=7)
####Evaluation
library("gmodels")
CrossTable(x=G_test_labels,y=G_n_pred,prop.chisq = FALSE)

#####Different k Values
library("class")
G_n_pred<-knn(train = G_train,test = G_test,cl=G_train_labels,k=9)
####Evaluation
library("gmodels")
CrossTable(x=G_test_labels,y=G_n_pred,prop.chisq = FALSE)


library("class")
G_n_pred<-knn(train = G_train,test = G_test,cl=G_train_labels,k=11)
####Evaluation
library("gmodels")
CrossTable(x=G_test_labels,y=G_n_pred,prop.chisq = FALSE)
