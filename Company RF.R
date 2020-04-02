D<-`Company_Data.(3)`[,]
View(D)
D$Sales<-as.character(D$Sales)
str(D$Sales)
D$CompPrice<-as.character(D$CompPrice)
D$Income<-as.character(D$Income)
D$Advertising<-as.character(D$Advertising)
D$Population<-as.character(D$Population)
D$Price<-as.character(D$Price)
D$Age<-as.character(D$Age)
D$Education<-as.character(D$Education)
D$Urban<-as.character(D$Urban)
D$US<-as.character(D$US)
D$ShelveLoc<-as.character(D$ShelveLoc)
str(D)
getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(D$Sales)
getmode(D$CompPrice)
getmode(D$Income)
getmode(D$Advertising)
getmode(D$Population)
getmode(D$Price)
getmode(D$ShelveLoc)
getmode(D$Age)
getmode(D$Education)
getmode(D$Urban)
getmode(D$US)
######Data is changed to Categorical so mode is only calculated.
D$Sales<-as.factor(D$Sales)

####table of taxable income
table(D$Sales)
# table or proportation of enteries in the datasets. 
round(prop.table(table(D$Sales)*100,1)
summary(D)
      #Create a function to normalize the data
      norm <- function(x){ 
        return((x-min(x))/(max(x)-min(x)))
      }
      #test normalization
      norm(c(1,2,3,4,5))
      norm(c(10,20,30,40,50))
      #Apply the normalization function to wbcd dataset
      D_n <- as.data.frame(lapply(D[,c(1,2,3,4,5,6,8,9)], norm))
      View(D_n)
      View(D)
      D_n["Sales"] <- D$Sales
      # Building a random forest model on training data 
      D_forest <- randomForest(Sales~.,data=D_n,importance=TRUE)
      plot(D_forest)
      ####Accuracy
      acc_D <- mean(D_n$Sales==predict(D_forest))
      acc_D
  ###Variable Importance PLOT
      varImpPlot(D_forest)
    