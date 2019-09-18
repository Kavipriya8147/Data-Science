######Simple Linear Regression########## 

#######1.calories_consumed Data########

library(readr)
calories_consumed <- read_csv("C:/Users/91829/Downloads/calories_consumed.csv")
View(calories_consumed)
boxplot(calories_consumed)
hist(calories_consumed$`Calories Consumed`)
model<-lm(calories_consumed$`Weight gained (grams)`~calories_consumed$`Calories Consumed`)
model$fitted.values
model$residuals
cor(model$fitted.values,calories_consumed$`Weight gained (grams)`)

########2.deliverytime Data############
library(readr)
delivery_time <- read_csv("C:/Users/91829/Downloads/delivery_time.csv")
View(delivery_time)
mk<-lm(delivery_time$`Delivery Time`~delivery_time$`Sorting Time`)
summary(mk)
mk$fitted.values
mk$residuals
cor(mk$fitted.values,delivery_time$`Delivery Time`)
hist(delivery_time$`Delivery Time`)
boxplot(delivery_time)

#########3.Salary Data#########
library(readr)
Salary_Data <- read_csv("C:/Users/91829/Downloads/Salary_Data.csv")
View(Salary_Data)
boxplot(Salary_Data)
hist(Salary_Data$Salary)
kavi<-lm(Salary_Data$Salary~Salary_Data$YearsExperience)
summary(kavi)
kavi$fitted.values
kavi$residuals
cor(kavi$fitted.values,Salary_Data$Salary)

###########4.emp_Data######
library(readr)
emp_data <- read_csv("C:/Users/91829/Downloads/emp_data.csv")
View(emp_data)
boxplot(emp_data)
hist(emp_data$Churn_out_rate)
sai<-lm(emp_data$Churn_out_rate~emp_data$Salary_hike)
summary(sai)
sai$fitted.values
sai$residuals
cor(sai$fitted.values,emp_data$Churn_out_rate)
