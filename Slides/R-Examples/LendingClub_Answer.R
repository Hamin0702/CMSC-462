library(psych)
library(dplyr)
library(ggplot2)
library(e1071)
library(tidyverse)
library(caret)

setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")
LendingClub = read.csv("Lending.csv")

summary(LendingClub)

hist(LendingClub$loan_default)
describe(LendingClub)
LendingClub$loan_default = factor(LendingClub$loan_default)
LendingClub$residence_property = factor(LendingClub$residence_property)



LendingClub2 = LendingClub

# 
LendingClub = rbind(sample_n(filter(LendingClub2, loan_default==1), 2000), sample_n(filter(LendingClub2, loan_default==0), 2000))
no_fac_lc = LendingClub


no_fac_lc$residence_property = NULL
no_fac_lc$loan_default = NULL

cor(no_fac_lc)
 
#pairs(LendingClub)

set.seed(123)

sample_size = floor(0.7 * nrow(LendingClub))

train_ind = sample(seq_len(nrow(LendingClub)), size = sample_size)

train_lc = LendingClub[train_ind, ]
test_lc = LendingClub[-train_ind, ]
#logit.fit = glm(loan_default ~  ., data = train_lc, family = "binomial")

# Full Model
#logit.fit = glm(loan_default ~  residence_property+loan_amnt+ adjusted_annual_inc+ pct_loan_income+ dti + months_since_first_credit 
         #         +inq_last_6mths + open_acc+ bc_util+ num_accts_ever_120_pd 
            #       + pub_rec_bankruptcies, data = train_lc, family = "binomial")
logit.fit =  glm(loan_default ~  residence_property+loan_amnt+ pct_loan_income+  inq_last_6mths
          + bc_util, data = train_lc, family = "binomial")



summary(logit.fit)

#set model threshold
myThreshold = .5
logit.pred = predict(logit.fit, newdata = test_lc, type = "response")
logit.cm = confusionMatrix(factor(as.numeric(logit.pred>myThreshold), levels = c(0,1)), reference = test_lc$loan_default)
logit.cm



#Answer for Q3
library(ROCR)
library(pROC) 
library(PRROC)


# Compute AUC for predicting Class with the model
prob <- predict(logit.fit, newdata=test_lc, type="response")
pred <- prediction(prob, test_lc$loan_default)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#following order: bottom, left, top, and right. 
par(mar=c(5,8,1,.5))
#Receiver operating characteristic
plot(perf, col="red")
abline(a=0, b=1)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

roc_obj <- roc(pred@labels[[1]], pred@predictions[[1]])
# Find the threshold that maximizes Youden's J statistic
coords(roc_obj, "best", ret="threshold", best.method="youden")

# Find the threshold that maximizes AUC
coords(roc_obj, "best", ret="threshold", best.method="closest.topleft")


#pr_obj <- pr.curve(pred@labels[[1]], pred@predictions[[1]])

#pr_obj$thresholds[which.max(pr_obj$F1)]

