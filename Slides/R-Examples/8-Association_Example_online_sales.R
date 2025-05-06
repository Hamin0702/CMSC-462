library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
 


#read excel into R dataframe
setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")

#OnlineRetail file describes 
retail <- read_excel("OnlineRetail.xlsx")
nrow(retail)
#complete.cases(data) will return a logical vector indicating which rows 
#have no missing values. Then use the vector to get only rows that 
#are complete using retail[,].
retail <- retail[complete.cases(retail), ]


retail$Description = as.factor(retail$Description)
retail$Country = as.factor(retail$Country)


#Converts character data to date. Store InvoiceDate as date in new variable
retail$Date <- as.Date(retail$InvoiceDate)
#Extract time from InvoiceDate and store in another variable
retail$TransTime<- format(retail$InvoiceDate,"%H:%M:%S")
#Convert and edit InvoiceNo into numeric
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))


#get a glimpse of your data
glimpse(retail)

 
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                  function(df1)paste(df1$Description, collapse = ","))
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
transactionData

#set column InvoiceNo of dataframe transactionData  
transactionData$InvoiceNo <- NULL
#set column Date of dataframe transactionData
transactionData$Date <- NULL
#Rename column to items
colnames(transactionData) <- c("items")
#Show Dataframe transactionData
transactionData

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
#transactionData: Data to be written

#quote: If TRUE it will surround character or factor column with double quotes. If FALSE nothing will be quoted
#row.names: either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.

tr <- read.transactions("market_basket_transactions.csv", format = 'basket', sep=',')
#sep tell how items are separated. In this case you have separated using ','

tr
summary(tr)
# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
#mar - A numeric vector of length 4, which sets the margin sizes in the 
#following order: bottom, left, top, and right. 
#The default is c(5.1, 4.1, 4.1, 2.1).
par(mar=c(0,10,1,.1))
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
association.rules <- apriori(tr, parameter = list(supp=0.01, conf=0.8,maxlen=10))

summary(association.rules)

inspect(association.rules[1:10])

