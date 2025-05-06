#install package tidyverse if necessary
library(tidyverse)
setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")

advertising = read.csv("Advertising.csv")

ggplot(data = advertising, mapping = aes(x = TV, y =  Sales)) +
  geom_point(color = "red") + ylim(c(0,30)) + # set limits for y-axis for better viewing
  labs(title = "Sales vs. TV", x = "TV", y = "Sales") + theme_light()

ggplot(data = advertising, mapping = aes(x = Radio, y =  Sales)) +
  geom_point(color = "blue") + ylim(c(0,30)) + # set limits for y-axis for better viewing
  labs(title = "Sales vs. Radio", x = "Radio", y = "Sales" ) + theme_light()

ggplot(data = advertising, mapping = aes(x = Newspaper, y =  Sales)) +
  geom_point(color = "black") + ylim(c(0,30)) + # set limits for y-axis for better viewing
  labs(title = "Sales vs. Newspaper", x = "Newspaper", y = "Sales") + theme_light()

# Fit the linear regression model and assign it to "MyModel"
MyModel = lm(Sales ~ Newspaper, data = advertising)

# Coefficient Estimates, t statistics, p-values, R-squared, 
# and residual standard error
summary(MyModel)

## ANOVA (Analysis of Variance) Table
anova(MyModel)

## Obtaining the Parameter Estimates
coef(MyModel)

## 95% Confidence Intervals for the Parameters
confint(MyModel, level = 0.95)

## 95% Confidence Interval for the Mean Value of Y Given X = x
predict(MyModel,newdata = data.frame(TV = 130), # x value entered as a data frame
        interval = "confidence")

# We can also obtain CIs for multiple values of dist
# Now dist is entered as a vector inside of a data frame
predict(MyModel, newdata = data.frame(TV = c(130, 140, 150)), interval = "confidence")

myFittedValue = predict(MyModel,
        newdata = data.frame(TV = advertising$TV), interval = "confidence")

myNewData = data.frame(myFittedValue, TV=advertising$TV)


ggplot(data = myNewData, mapping = aes(x = TV, y = fit)) +
    geom_line(aes(x = TV, y = fit ), color = "blue", size = .5) +
    geom_line(aes(x = TV, y = lwr ), color = "black", size = 1)+
    geom_line(aes(x = TV, y = upr ), color = "black", size = 1)+
    geom_point(aes(x = TV, y = advertising$Sales), color = "red", size = 1) +
    labs(title = "TV vs Sales",  x = "TV",  y = "Sales") +  theme_light()
  
  