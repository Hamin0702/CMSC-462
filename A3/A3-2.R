library(tidyverse)

# Import dataset
setwd("C:/Users/hamin/Desktop/CMSC462/A3")
lending_data <- read_csv("Lending.csv")

# Fit the linear regression model and assign it to "MyModel"
MyModel = lm(loan_default ~ . , data = lending_data)

# Coefficient Estimates, t statistics, p-values, R-squared, and residual standard error
summary(MyModel)


columnsChange = c("residence_property")

#sapply allows you to loop through and applies the function to each column
sapply(lending_data[columnsChange], unique)
#lapply allows you to loop through, in addition it returns
# a list or dataframe
lending_data[columnsChange]  = lapply(lending_data[columnsChange], as.factor)
glimpse(lending_data)
#Separating Test and Training Data
TrainIndex = sample(1:nrow(lending_data), round(0.7*nrow(lending_data)))
loanTrain = lending_data[TrainIndex, ] 
loanTest = lending_data[-TrainIndex, ] 

# Logistic Regression
LoanLogit = glm(loan_default ~ ., data = loanTrain,# same as in lm()
                 family = "binomial") # for logistic, this is always set to "binomial"



summary(LoanLogit)

varImp(LoanLogit, scale=FALSE)
#creates a new column called EstimatedProb in loanTest
EstimatedProb = predict(LoanLogit,
                        newdata = loanTest, type = "terms")

loanTest = loanTest %>% 
  mutate(EstimatedProb = predict(LoanLogit,
                                 newdata = loanTest, type = "response"))
summary(loanTest$EstimatedProb)


library(ROCR)
library(pROC) 
library(PRROC)

# Compute AUC for predicting Class with the model
prob <- predict(LoanLogit, newdata=loanTest, type="response")
pred <- prediction(prob, loanTest$loan_default)

#perf contains the data for drawing ROC curve 
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

#Check how the values have been calculated
temp = data.frame(loan_default = as.vector(pred@labels[[1]]))
count(temp, loan_default)
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


# Naive Bayes

LoanNV <- naiveBayes(loan_default ~ ., data = loanTrain)
preds <- predict(LoanNV, newdata = loanTest)
conf_matrix <- table(preds, loanTest$loan_default)

conf_matrix
confusionMatrix(conf_matrix)

## check the raw, this gives you the probability 
predsRaw <- predict(LoanNV, newdata = loanTest, type = "raw")
predsRaw


#Examine NVModelin  details

LoanNV
LoanNV$tables

LoanNV$apriori

# Compute AUC for predicting Class with the model
prob <- predict(LoanNV, newdata=loanTest, type="raw")
pred <- prediction(prob[,2], loanTest$loan_default)
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
