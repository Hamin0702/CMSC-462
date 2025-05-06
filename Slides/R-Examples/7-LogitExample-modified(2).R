library(tidyverse)
library(readxl)
library(caret)

setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")

Heart = read_excel("Heart Disease.xlsx")



# Logistic Regression

# The I() function creates a logical vector that is TRUE when HeartDisease is "Yes"
# and FALSE otherwise
Heart = Heart %>% mutate(HeartDisease = I(HeartDisease == "Yes") %>% as.numeric())
#Separating Test and Training Data


columnsChange = c("Sex", "ChestPain", "Fbs", "RestECG", "ExAng", "Slope","ThalliumStressTest")

#sapply allows you to loop through and applies the function to each column
sapply(Heart[columnsChange], unique)
#lapply allows you to loop through, in addition it returns
# a list or dataframe
Heart[columnsChange]  = lapply(Heart[columnsChange], as.factor)
glimpse(Heart)
TrainIndex = sample(1:nrow(Heart), round(0.7*nrow(Heart)))
HeartTrain = Heart[TrainIndex, ] 
HeartTest = Heart[-TrainIndex, ] 

# Here you can try  
HeartLogit = glm(HeartDisease ~ ., data = HeartTrain,# same as in lm()
                 family = "binomial") # for logistic, this is always set to "binomial"



summary(HeartLogit)

varImp(HeartLogit, scale=FALSE)
#creates a new column called EstimatedProb in HeartTest
EstimatedProb = predict(HeartLogit,
                        newdata = HeartTest, type = "terms")

HeartTest = HeartTest %>% 
  mutate(EstimatedProb = predict(HeartLogit,
            newdata = HeartTest, type = "response"))
summary(HeartTest$EstimatedProb)


# Now let's predict Y = 1 if P(Y = 1) > 0.5
HeartTest2 = HeartTest %>% mutate(HeartLogitPredicited = I(EstimatedProb > 0.5) %>% as.numeric())
glimpse(HeartTest2)

heartTable = table(HeartTest2$HeartLogitPredicited ,HeartTest2$HeartDisease)

confusionMatrix(heartTable)



 
library(ROCR)
library(pROC) 
library(PRROC)

# Compute AUC for predicting Class with the model
prob <- predict(HeartLogit, newdata=HeartTest, type="response")
pred <- prediction(prob, HeartTest$HeartDisease)

#perf contains the data for drawing ROC curve 
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

#Check how the values have been calculated
temp = data.frame(HeartDisease = as.vector(pred@labels[[1]]))
count(temp, HeartDisease)
#The cutoff values
pred@cutoffs[[1]]

#following order: bottom, left, top, and right. 
par(mar=c(5,8,1,.5))
#Receiver operating characteristic
plot(perf, col="red")
abline(a=0, b=1)
auc <- performance(pred, measure = "auc")
auc@y.values[1]
auc
##############################################
roc_obj <- roc(pred@labels[[1]], pred@predictions[[1]])
# Find the threshold that maximizes Youden's J statistic
coords(roc_obj, "best", ret="threshold", best.method="youden")

# Find the threshold that maximizes AUC
coords(roc_obj, "best", ret="threshold", best.method="closest.topleft")


pr_obj <- pr.curve(pred@labels[[1]], pred@predictions[[1]])

pr_obj$thresholds[which.max(pr_obj$F1)]

 
 
