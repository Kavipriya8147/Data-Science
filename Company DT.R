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
#####DT using C5.0 package
model=C5.0(D$Sales~.,data = D)
summary(model)
pred=predict.C5.0(model,D[,-1])
table(D$Sales,pred)


acc=c()
for (i in 1:10){
  splitratio=createDataPartition(D$Sales,p=0.85,list = FALSE)
train1=D[splitratio,]
test1=D[-splitratio,]
fittree=C5.0(train1$Sales~.,data=train1)
pred2=predict.C5.0(fittree,test1[-1])
b=table(test1$Sales,pred2)
acc=c(acc,sum(diag(b)/sum(b)))
}
summary(acc)
