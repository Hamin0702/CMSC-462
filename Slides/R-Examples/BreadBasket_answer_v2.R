#install and load package arules
#install.packages("arules")
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)


#read excel into R dataframe
setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")

basket <- read_xlsx("BreadBasket_DMS.xlsx")
nrow(basket)

basket <- basket[complete.cases(basket), ]
#view(basket)

#remove rows where item = 'NONE'
basket <- basket[!(basket$Item=="NONE"),]

nrow(basket)

basket %>% mutate(Item = as.factor(Item))





transactionNum <- as.numeric(as.character(basket$Transaction))
cbind(basket,transactionNum)

glimpse(basket)

saleData <- ddply(basket, c("transactionNum"),
                  function(df1)paste(df1$Item, collapse = ","))
saleData

saleData$transactionNum <- NULL

saleData

write.csv(saleData,"sale_data.csv", quote = FALSE, row.names = FALSE)
########### 

saleData_df = read_csv("sale_data.csv", col_names = FALSE )
glimpse(saleData_df)

sd <- read.transactions("sale_data.csv", format = 'basket', sep = ',')

sd

summary(sd)

if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(sd,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#Generate the Association rules

association.rules <- apriori(sd, parameter = list(supp=0.001, conf=0.4,minlen=2))

count(basket$Item)

itemsHere <- unique(basket$Item)

view(itemsHere)

count(itemsHere)

#
SubRulesNoCoffee = subset(association.rules, subset = !(rhs %in% "Coffee"))
summary(SubRulesNoCoffee)
inspect(SubRulesNoCoffee)
inspect(sort(SubRulesNoCoffee, decreasing=TRUE,by="lift"))
SubRulesCoffee = subset(association.rules, subset = rhs %in% "Coffee")
summary(SubRulesCoffee)
inspect(sort(SubRulesCoffee, decreasing=TRUE,by="lift")[1:10])
 
 