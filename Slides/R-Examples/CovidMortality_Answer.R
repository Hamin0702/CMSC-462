library(tidyverse)
library(readxl)

setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")
covid = read_excel("CovidMortalityModified.xlsx")

unique(covid$StateInd)
covid$StateInd = as.factor(covid$StateInd)

covid$State = NULL
covidNumer = covid
covidNumer$StateInd = NULL
cor (covidNumer)
DeathModel = lm(Deaths ~ Confirmed + StateInd , data = covid)
summary(DeathModel)
anova(DeathModel)   
