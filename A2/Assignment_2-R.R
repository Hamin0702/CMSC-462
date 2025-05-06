### R Assignment 1

### Run the following R code and add code/commented answers where prompted


######################################## 1. #############################################
myVar = "83"

# Write the code that changes myVar to an integer data type. Assign the output to myVar.
myVar <- as.integer(myVar)
# Check the class of myVar
class(myVar)


######################################## 2. #############################################
# Create a vector named NumVec1 with the following entries -1, 4.5, 9, 10, 3
NumVec1 <- c(-1,4.5,9,10,3)

# Create a vector named NumVec2 with the following entries -5, 10, -2, 20, 3
NumVec2 <- c(-5, 10, -2, 20, 3)

# Write the code that calculates the sum of NumVec1 and NumVec2
NumVec1 + NumVec2

# Write the relational expression that checks where NumVec1 is greater than NumVec2
NumVec1 > NumVec2

# Write the relational expression that finds where NumVec1 has the same value as NumVec2
NumVec1 == NumVec2

# Update the NumVec1 vector to equal 1 if NumVec1 is less than 5
# and 0 if NumVec1 is greater than or equal to 5
NumVec1[NumVec1 < 5] <- 1
NumVec1[NumVec1 >= 5] <- 0

# Print the the NumVec1 vector to check your results
# Hint: NumVec1 should now be [1, 1, 0, 0, 1]
print(NumVec1)

########################################## 3. #############################################
TextVar = "CMSC 462"
Data = c(3, 7, 4.5, 9, 4, 5.67)

# Write an IF statement that prints "CMSC 462 is assigned to TextVar" if TextVar is equal to "CMSC 462"
if (TextVar == "CMSC 462"){
  print("CMSC 462 is assigned to TextVar")
}


# Write an IF-ELSE statement that prints "The mean is less than 5" if the mean of the 
# Data vector is less than 5, or prints "The mean is greater than or equal to 5" if not
# Hint: the mean() function calculates the mean of a vector
if (mean(Data) < 5) {
  print("The mean is less than 5")
} else {
  print("The mean is greater than or equal to 5")
}


# Write a FOR loop that prints the square root of the elements in the Data vector
# the sqrt() function calculates the square root of a number
for (x in Data) {
  print(sqrt(x))
}


########################################## 4. #############################################
myList = list(Gender = c("M", "F"),
              Age = c(26, 24),
              City = c("Catonsville","Towson"))

# Write the code that gives the mean of the Age vector in myList
mean(myList$Age)

# Write the code that gives the second element of the City vector in myList
myList$City[2]

# Write the code that gives the value of City in myList that corresponds to a value of "M" in Gender
myList$City[which(myList$Gender == "M")]

########################################## 5. #############################################
myDataframe = data.frame(Model = c("Toyota","Honda","Ford"),
                         mpg = c(34, 33, 22),
                         stringsAsFactors = FALSE)

# Write the command that gives the first row, second column of myDataframe
myDataframe[1, 2]


# Write the command that gives the average of the mpg column in myDataframe
mean(myDataframe$mpg)

# Write the command that gives the Model value that corresponds to an mpg value of 22 in myDataframe
subset(myDataframe, mpg == 22)$Model

################################# Question 6 ############################################

# Run the code below to import the tidyverse package
library(tidyverse)

# Fill in the correct path to the Tipping Data.txt file on your computer
# Remember to end your file path with Tipping Data.txt (the name of the file)
# Separate parts of your path with "/" 

Tipping = read_tsv(file = "C:/Users/hamin/Desktop/CMSC462/A2/Tipping Data.txt" )

# Now you should have a data frame, Tipping, available in your R session
# Run the code below to see what's stored in Tipping
# Tipping contains customer tipping data for a California restaurant
glimpse(Tipping)

################################# Question 7 ############################################

# We will be using dplyr functions to answer Question 7
# Here is an example of what I expect your answer to be

# Example question: Use filter() to subset the Tipping data
# to contain only Male customers.

# Your answer should be:
filter(Tipping, sex == "Male")

# Use filter() to subset the Tipping data
# to contain only Male customers who ate dinner on Saturday or Sunday

# Hint: the "day" variable gives the day a customer ate at the restuarant
# the "time" variable gives whether the meal was Dinner or Lunch
 

# Enter your code below:
filter(Tipping, sex == "Male" & (day == "Sat" | day == "Sun"))



# Use filter() to subset the Tipping data
# to contain only customers that tipped more than 6 dollars

# Hint: the "tip" variable contains the dollar amount that a 
# customer left as tip

# Enter your code below:

filter(Tipping, tip > 6)

# Use arrange() to sort the Tipping data frame
# by total_bill and then by tip, both in DESCENDING order

# Enter your code below:
Tipping %>% arrange(desc(total_bill))
Tipping %>% arrange(desc(tip))

################################# Question 8 ############################################

# Use select() to select the first 2 columns of Tipping

# Enter your code below:
Tipping %>% select(1:2)

# Use select() to select columns that contain the lower case letter "s"

# Enter your code below:
Tipping %>% select(contains("s"))

# Use summarise() to obtain the average tip amount
# Assign the result to a variable called "AvgTip" within the summarise function

# Enter your code below:
Tipping %>% summarise(AvgTip = mean(tip))

# Enter the numeric value that you got from the code above, what's the average tip amount?
# Your answer: 3


################################# Question 9 ############################################

# For question 9, you will need to use the %>% operator for your answers

# Use group_by() and summarise() to obtain the average tip by customer gender
# Assign the results to a variable named AvgTip, just like you did above

# Hint: Your code should look like this: Tipping %>% group_by() %>% summarise()
# Just fill in the correct input 

# Enter your code below:
Tipping %>% group_by(sex) %>% summarise(AvgTip = mean(tip))

# From the results above, which gender has higher tips, on average? Just enter male or female.
# Your answer: male 

# Use mutate() to compute a variable called TipPercent which equals tip/total_bill
# Hint, fill in the correct input: Tipping %>% mutate()

# Enter your code below:
Tipping %>% mutate(TipPercent = tip/total_bill)


# Now extend the analysis above: Use a combination of mutate(), group_by(), and summarise()
# to compute the average tip percent by customer gender. Assign the results to TipPercent 
# within the mutate() function, just like above, and AvgTipPct
# within the summarise() function

# Hint: mutate() first, then group_by(), then summarise(), use the %>% operator
# Within summarise, you need to input AvgTipPct = mean(). What should go inside the mean() function?

# Enter your code below:
Tipping %>% mutate(TipPercent = tip/total_bill) %>% group_by(sex) %>% summarise(AvgTipPct = mean(TipPercent))



################################# Question 10 ############################################

# Write the R code that uses ggplot2 to produce a histogram
# of the tip variable in Tipping
# Set the color of the bars to "blue", and the border
# of the bars to "white"
# Add an appropriate title and x/y axis labels




# Enter your code below:
ggplot(Tipping, aes(x=tip)) + geom_histogram(color="white", fill="blue") +labs(title = "Tips", x = "Tip Amount ($)", y = "Count")
