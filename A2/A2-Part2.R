# Run the code below to import the tidyverse package
library(tidyverse)

# Import the excel file
data = read_csv(file = "C:/Users/hamin/Desktop/CMSC462/A2/myData.csv" )

# Take a glipse
glimpse(data)

# Standard deviation for x
x_stddev <- sd(data$x)

# Standard deviation for y
y_stddev <- sd(data$y)

# Actual mean for x 
x_mean = mean(data$x)

# Actual mean for y 
y_mean = mean(data$y)

# Point estimate for x 
x_point_estimate_mean = mean(sample(data$x,100))

# Point estimate for y 
y_point_estimate_mean = mean(sample(data$y,100))
