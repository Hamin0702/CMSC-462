library(mlbench)
library(e1071)
 
library(tidyverse)
library(readxl)
library(caret)

setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")

heartFailure = read_excel("heartFailure.xlsx")
glimpse(heartFailure)
#Now select columns to change to factor

columnsChange = c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking","DEATH_EVENT")

#sapply allows you to loop through and applies the function to each column
sapply(heartFailure[columnsChange], unique)
#lapply allows you to loop through, in addition it returns
# a list or dataframe
heartFailure[columnsChange]  = lapply(heartFailure[columnsChange], as.factor)

glimpse(heartFailure)
 
## 75% of the sample size
smp_size <- floor(0.75 * nrow(heartFailure))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(heartFailure)), size = smp_size)

train <- heartFailure[train_ind, ]
test <- heartFailure[-train_ind, ]

NVmodel <- naiveBayes(DEATH_EVENT ~ ., data = train)
preds <- predict(NVmodel, newdata = test)
conf_matrix <- table(preds, test$DEATH_EVENT)

conf_matrix
confusionMatrix(conf_matrix)

## check the raw, this gives you the probability 
predsRaw <- predict(NVmodel, newdata = test, type = "raw")
predsRaw


#Examine NVModelin  details

NVmodel
NVmodel$tables

NVmodel$apriori

table (train$DEATH_EVENT[train$high_blood_pressure == 0] == 1)
table(train$DEATH_EVENT)

#Check how the probability was calculated
#count(train, train$DEATH_EVENT) 
temp = as.data.frame (count(train, train$DEATH_EVENT, train$anaemia))
myMatrix = matrix(as.data.frame (count(train, train$DEATH_EVENT, train$anaemia))[,3], nrow=2, byrow=TRUE)
rownames(myMatrix) = c("DEATH_EVENT=0", "DEATH_EVENT=1")          
colnames(myMatrix) = c("anaemia=0", "anaemia=1")    

myMatrix

myMatrix/(myMatrix[,1]+myMatrix[,2])

 
mean(train$age[train$DEATH_EVENT== 1])
sd(train$age[train$DEATH_EVENT== 1])


library(ROCR)


# Compute AUC for predicting Class with the model
prob <- predict(NVmodel, newdata=test, type="raw")
pred <- prediction(prob[,2], test$DEATH_EVENT)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#following order: bottom, left, top, and right. 
par(mar=c(5,8,1,.5))
#Receiver operating characteristic
plot(perf, col="red")
abline(a=0, b=1)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

rocData = data.frame(c(perf@alpha.values, perf@x.values, perf@y.values))

