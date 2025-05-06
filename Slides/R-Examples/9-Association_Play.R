# Import the following libraries
library(tidyverse)
library(readxl)
#If you do not have arules installed then please use command install.packages("arules")
library(arules)

# Discovering Associations:  

# Let's read in the Play Dataset
setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")

Play = read_excel(path = "./rainPlay.xlsx")
 

# We also have to recode our character variables to factors
# The lapply function is used to execute a function on multiple elements
# of a list, remember that dataframe are a special type of list
Play[] = lapply(Play[],
           function(Col) {factor(Col, levels = unique(Col),labels = unique(Col)) } )


# Now we can find the association rules in the data set
Rules = apriori(Play, 
                parameter = list(confidence = 0.75, minlen=4)) # Set minimum confidence level
inspect(sort(Rules, decreasing = TRUE, by = "lift")) 
