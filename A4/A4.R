library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)



#read excel into R dataframe
setwd("C:/Users/hamin/Desktop/CMSC462/A4")

#OnlineRetail file describes 
bread <- read_csv("BreadBasket_DMS.csv")
nrow(bread)
#complete.cases(data) will return a logical vector indicating which rows 
#have no missing values. Then use the vector to get only rows that 
#are complete using bread[,].
bread <- bread[complete.cases(bread), ]

#get a glimpse of your data
glimpse(bread)

#########################
# Descriptive Statistics

# List all unique items
unique_items <- unique(bread$Item)

# Count the frequency of each item
item_counts <- table(bread$Item)

# Sort the items by frequency in descending order
item_counts <- item_counts[order(-item_counts)]

# Print the distinct items and their frequencies
print(unique_items)
print(item_counts)

###############
# Market Basket analysis


#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(bread,c("Transaction","Item"),
                         function(df1)paste(df1$Description, collapse = ","))

write.csv(transactionData,"market_basket_bread.csv", quote = FALSE, row.names = FALSE)
#transactionData: Data to be written

#quote: If TRUE it will surround character or factor column with double quotes. If FALSE nothing will be quoted
#row.names: either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.

tr <- read.transactions("market_basket_bread.csv", format = 'basket', sep=',')
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

##################
# Top five rules, support, confidence, lift


association.rules <- apriori(tr, parameter = list(supp = 0.01, conf = 0.8, maxlen = 4))
filtered_rules <- subset(rules, rhs %in% "Coffee")

# Sort the rules by lift and select the top 5
top_rules <- head(sort(filtered_rules, by = "lift", decreasing = TRUE), 5)

# Describe the top rules
summary(top_rules)


