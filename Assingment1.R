Basic Visualizations

######cars.csv Data is Taken.

cars<-Cars[,]
#########Histogram is used to show frequency distributions.
hist(cars$HP)
hist(cars$MPG)
hist(cars$VOL)
hist(cars$SP)
hist(cars$WT)
##########Boxplot is used for finding Outliers.

boxplot(cars$HP)
boxplot(cars$WT)
boxplot(cars$MPG)
boxplot(cars$SP)
boxplot(cars$VOL)



############Computer_Data#########
A<-Computer_Data[,]

#########Histogram is used to show frequency distributions.
hist(A$X)
hist(A$price)
hist(A$speed)
hist(A$hd)
hist(A$ram)
hist(A$screen)
hist(A$ads)
##########Boxplot is used for finding Outliers.
boxplot(A$X)
boxplot(A$price)
boxplot(A$speed)
boxplot(A$hd)
boxplot(A$ram)
boxplot(A$screen)
boxplot(A$ads)

###########Q7.csv########
B<-Q7[,]

#########Histogram is used to show frequency distributions.
hist(B$Points)
hist(B$Score)
hist(B$Weigh)
##########Boxplot is used for finding Outliers.
boxplot(B$Points)
boxplot(B$Score)
boxplot(B$Weigh)


#################Q9_a.csv###########
D<-Q9_a[,]
#########Histogram is used to show frequency distributions.
hist(D$speed)
hist(D$dist)
##########Boxplot is used for finding Outliers.
boxplot(D$speed)
boxplot(D$dist)
 

##########Q9_b.csv########
E<-Q9_b[,]
#########Histogram is used to show frequency distributions.
hist(E$SP)
hist(E$WT)
##########Boxplot is used for finding Outliers.
boxplot(E$SP)
boxplot(E$WT)


##########Wc.at#######
F<-wc.at[,]
#########Histogram is used to show frequency distributions.
hist(F$Waist)
hist(F$AT)
##########Boxplot is used for finding Outliers.
boxplot(F$Waist)
boxplot(F$AT)

