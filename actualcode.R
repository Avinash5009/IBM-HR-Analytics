#set up working directory#
setwd("E:/IBM HR analytics_Exam")
getwd()

#import the data 
data = read.csv("Attrition.csv", header = TRUE, sep = ",", na.strings = (""))

#since the actual ask of the problem is that we have to find the attrition of why the employees are leaving the 
#organisation, so we need to analyse the data of our target variable with all the possible other independent variables

#identifyting the missing values 
library(Hmisc)
names(data) #checking for any improper name variables
describe(data) #derived from Hmisc library as it gives us all the measures of central tendency 
sum(is.na(data)) #there is no missing values in the data
str(data)

#split the data into train and test

dim(data)
install.packages("caTools") #using the caTools library for splitting the data
library(caTools)
set.seed(123) #we set seed so that some random numbers are generated
sample = sample.split(data,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. 
#After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE

train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

#now we perform the data analysis of all the variables with the main target variable attrition

str(train)
library(dplyr)
library(e1071)
library(ggplot2)

#initially we convert the data types which are integer in nature to factors as few of them are categorical in nature

train$Education <- as.factor(train$Education)
train$EnvironmentSatisfaction <- as.factor(train$EnvironmentSatisfaction)
train$JobInvolvement <- as.factor(train$JobInvolvement)
train$JobLevel <- as.factor(train$JobLevel)
train$JobSatisfaction <- as.factor(train$JobSatisfaction)
train$PerformanceRating <- as.factor(train$PerformanceRating)
train$RelationshipSatisfaction <- as.factor(train$RelationshipSatisfaction)
train$StandardHours <- as.factor(train$StandardHours)
train$StockOptionLevel <- as.factor(train$StockOptionLevel)
train$WorkLifeBalance <- as.factor(train$WorkLifeBalance)
str(train) #all the required variables have been converted to factors other than few continous variables

#In the data set we can see some of the variables have only one factor which is common
#Among all the employees, so these variables can be omitted out

#the below variables can be dropped as these have one factors only

library(dplyr)
train_dropped <- select(train,-c(EmployeeCount,Over18,StandardHours))
str(train_dropped)

#we check the exploratory data analysis steps for the new data set after omitting the values

boxplot(train_dropped)
describe(train_dropped)
summary(train_dropped)

#performing exploratory data analysis
#we perform a series of plots of certain variables and compare them with our target variable attrition

#scatter plot between monthly income, work life balance and attrition
ggplot(data,aes(data$MonthlyIncome,data$WorkLifeBalance, color=Attrition))+geom_point()

#scatter plot between monthly income, JobLevel and attrition
ggplot(data,aes(data$MonthlyIncome,data$JobLevel, color=Attrition))+geom_point()


#boxplot between monthly income and attrition
ggplot(data,aes(Attrition,MonthlyIncome,fill=Attrition))+geom_boxplot()


#violin plot between monthly income and attrition
ggplot(data,aes(Attrition,YearsSinceLastPromotion,fill=Attrition))+geom_violin()

#we check the correlation of the entire data set
install.packages("corrplot")
library(corrplot)

##logistic regression##

model <- glm(Attrition ~Age+BusinessTravel+Department+DistanceFromHome+
               Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+
               JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
               OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
               TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+ 
               YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
               YearsWithCurrManager, family ='binomial',data = train_dropped)
summary(model)
train_dropped$preds <-predict(model,train_dropped,type = 'response')
View(train_dropped$preds)
train_dropped$outcome <-ifelse(train_dropped$preds>=0.5,1,0)
table(train_dropped$Attrition,train_dropped$outcome)
#accuracy is 97% on train data

##random forest##
library(randomForest)
model_rf <- randomForest(Attrition ~Age+BusinessTravel+Department+DistanceFromHome+
                           Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+
                           JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
                           OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                           TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+ 
                           YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                           YearsWithCurrManager,data=train_dropped)
model_rf
#accuracy is 85% on train data


##decision trees##

library(party)
model_tree <- ctree(Attrition ~Age+BusinessTravel+Department+DistanceFromHome+
                      Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+
                      JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
                      OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                      TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+ 
                      YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                      YearsWithCurrManager,data=train_dropped)
plot(model_tree)
dev.off()
summary(model_tree)
model_tree
train_dropped$preds_modeltree<- predict(model_tree,train_dropped)
table(train_dropped$Attrition,train_dropped$preds_modeltree)
#accuracy is 84% on train data


##support vector machines##
model_svm <- svm(Attrition ~Age+BusinessTravel+Department+DistanceFromHome+
                   Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+
                   JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
                   OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                   TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+ 
                   YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                   YearsWithCurrManager,data=train_dropped)
model_svm
train_dropped$preds <-predict(model_svm,train_dropped)
table(train_dropped$Attrition,train_dropped$preds)
#accuracy is 84% on test data

##now we use the same on test data##

str(test)
test$Education <- as.factor(test$Education)
test$EnvironmentSatisfaction <- as.factor(test$EnvironmentSatisfaction)
test$JobInvolvement <- as.factor(test$JobInvolvement)
test$JobLevel <- as.factor(test$JobLevel)
test$JobSatisfaction <- as.factor(test$JobSatisfaction)
test$PerformanceRating <- as.factor(test$PerformanceRating)
test$RelationshipSatisfaction <- as.factor(test$RelationshipSatisfaction)
test$StandardHours <- as.factor(test$StandardHours)
test$StockOptionLevel <- as.factor(test$StockOptionLevel)
test$WorkLifeBalance <- as.factor(test$WorkLifeBalance)

#the below variables can be dropped as these have one factors only

library(dplyr)
test_dropped <- select(test,-c(EmployeeCount,Over18,StandardHours))
str(test_dropped)

##logistic regression##

model <- glm(Attrition~Age+BusinessTravel+Department+DistanceFromHome+
               Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+
               JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
               OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
               TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+ 
               YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
               YearsWithCurrManager, family ='binomial',data = test_dropped)
summary(model)
test_dropped$preds <-predict(model,test_dropped,type = 'response')
View(test_dropped$preds)
test_dropped$outcome <-ifelse(test_dropped$preds>=0.5,1,0)
table(test_dropped$Attrition,test_dropped$outcome)
#accuracy is 92%

##random forest##
library(randomForest)
model_rf <- randomForest(Attrition ~Age+BusinessTravel+Department+DistanceFromHome+
                           Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+
                           JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
                           OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                           TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+ 
                           YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                           YearsWithCurrManager,data=test_dropped)
model_rf
#accuracy is 84% on test data


##decision trees##

library(party)
model_tree <- ctree(Attrition ~Age+BusinessTravel+Department+DistanceFromHome+
                      Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+
                      JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
                      OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                      TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+ 
                      YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                      YearsWithCurrManager,data=test_dropped)
plot(model_tree)
dev.off()
summary(model_tree)
model_tree
test_dropped$preds_modeltree<- predict(model_tree,test_dropped)
table(test_dropped$Attrition,test_dropped$preds_modeltree)
#accuracy is 84% on train data


##support vector machines##
model_svm <- svm(Attrition ~Age+BusinessTravel+Department+DistanceFromHome+
                   Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+
                   JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+
                   OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                   TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+ 
                   YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                   YearsWithCurrManager,data=test_dropped)
model_svm
test_dropped$preds <-predict(model_svm,test_dropped)
table(test_dropped$Attrition,test_dropped$preds)
#accuracy is 84% on test data


