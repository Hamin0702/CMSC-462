# Import the following libraries
library(tidyverse)
library(readxl)
#If you do not have arules installed then please use command install.packages("arules")
library(arules)

# Discovering Associations: An Example with the Heart Disease Data
setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")
# Let's read in the Heart Disease Dataset
 
Heart = read_excel(path = "./Heart Disease.xlsx")
# Select categorical variables
HeartCategorical = Heart %>% select(HeartDisease,ChestPain,ThalliumStressTest,Sex)

# Recode the Sex variable to be a factor
HeartCategorical = HeartCategorical %>% 
  mutate(Sex = factor(Sex, levels = c(0,1), labels = c("Female","Male")))

# We also have to recode our character variables to factors
# The lapply function is used to execute a function on multiple elements
# of a list, remember that dataframe are a special type of list
HeartCategorical[,c(1:3)] = lapply(HeartCategorical[,c(1:3)],
           function(Col) {factor(Col, levels = unique(Col),labels = unique(Col)) } )


# Now we can find the association rules in the data set
Rules = apriori(HeartCategorical, 
                parameter = list(confidence = 0.85)) # Set minimum confidence level

# Store results in a dataframe
Results = as(Rules,"data.frame")

# To see what variables we have available, use the names() function
names(Results)

# Let's see the rules that were found in our data
# All of these have at least 0.85 confidence
Results %>% select(rules, confidence) %>% arrange(desc(confidence))

# Let's find rules with a lower confidence threshold
Rules = apriori(HeartCategorical, 
                parameter = list(confidence = 0.6))

# Now let's focus on a subset of the rules
# For this we have to use the subset function
SubRules = subset(Rules, subset = rhs %in% c("HeartDisease=Yes","HeartDisease=No"))

# Turn our results into a dataframe
SubResults = as(SubRules,"data.frame")

# View Results
SubResults %>% select(rules, confidence) %>% arrange(desc(confidence))


# Including Numeric Variables
HeartMixed = Heart %>% 
select(HeartDisease,ChestPain,ThalliumStressTest,Sex,MaxHR,RestBP)

# We still have to code our character vectors as factors
HeartMixed = HeartMixed %>%  mutate(Sex = factor(Sex, levels = c(0,1),
                   labels = c("Female","Male")))

# We also have to recode our character variables to factors
# The lapply function is used to execute a function on multiple elements
# of a list, remember that dataframe are a special type of list
HeartMixed[,c(1:3)] = lapply(HeartMixed[,c(1:3)], function(Col) {factor(Col,
         levels = unique(Col), labels = unique(Col)) } )

# Now we recode our numeric variables to indicator variables
HeartMixed = HeartMixed %>% 
         mutate(MaxHR = I(MaxHR >= median(MaxHR)) %>% as.numeric(),
         RestBP = I(RestBP >= median(RestBP)) %>% as.numeric())

# Now we recode our indicator variables to factors with 1 = "Yes", 0 = "No"
HeartMixed[,c(5:6)] = lapply(HeartMixed[,c(5:6)],
    function(Col) {factor(Col, levels = c(0,1), labels = c("Low","High"))})

# This time we use the default 0.8 minimum confidence
RulesMixed = apriori(HeartMixed)

# Now let's focus on a subset of the rules
# For this we have to use the subset function
SubRulesMixed = subset(RulesMixed, subset = rhs %in% c("HeartDisease=Yes","HeartDisease=No"))
mySubRules =setdiff(RulesMixed, SubRulesMixed)

 
summary(SubRulesMixed)  

inspect(SubRulesMixed)
inspect(sort(SubRulesMixed, decreasing = TRUE, by = "lift"))
