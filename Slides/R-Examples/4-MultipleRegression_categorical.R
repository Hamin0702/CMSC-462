library(tidyverse)


############################## Categorical Predictor Example ####################################
# Import iris dataset



# Let's see the distribution of Sepal.Length by Species of plant
ggplot(data = iris, mapping = aes(x = Sepal.Length)) +
  geom_histogram(aes(fill = Species), color = "white", bins = 15) +
  facet_wrap(~Species, nrow = 3) +
  labs(title = "Distribution of Sepal Length by Species",
       x = "Sepal Length", y = "Number of Plants") +  theme_light()

# Let's see the relationship between Sepal.Length and Sepal.Width
ggplot(data = iris, mapping = aes(y = Sepal.Length, x = Sepal.Width)) +
  geom_point(aes(color = Species), size = 2) +
  labs(title = "Sepal Length by Sepal Width Colored by Species of Plant",
       x = "Sepal Width",
       y = "Sepal Length") +
  theme_light()

# Add linear regression lines by Species, by default this includes interaction terms
# since the slopes can be different for each category
# However in our example, the assumption of equal slopes seems reasonable
ggplot(data = iris, mapping = aes(y = Sepal.Length, x = Sepal.Width)) +
  geom_point(aes(color = Species), size = 2) +
  geom_smooth(aes(color = Species), method = "lm", se = FALSE) +
  labs(title = "Sepal Length by Sepal Width Colored by Species of Plant",
       x = "Sepal Width",
       y = "Sepal Length") +
  theme_light()
class(iris$Species)

# Now we fit a multiple linear regression that predicts Sepal.Length
# by Species and Sepal.Width
#SepalLengthModel = lm(Sepal.Length ~ Species + Sepal.Width, data = iris)
SepalLengthModel = lm(Sepal.Length ~ ., data = iris)

# Let's see the summary output
summary(SepalLengthModel)

# Let's build a data frame that contains the original data, residuals, and predictions
ModelOutput = data.frame(iris, # include the original data set
                         Residuals = SepalLengthModel$residuals,
                         Predictions = SepalLengthModel$fitted.values)

# Let's look at the histograms of the model residuals, by Species
ggplot(data = ModelOutput, mapping = aes(x = Residuals)) +
  geom_histogram(aes(fill = Species), color = "white", bins = 15) +
  facet_wrap(~ Species, nrow = 3) +
  labs(titlw = "Model Residuals by Species",
       y = "Number of Observations",
       x = "Residuals") +
  theme_light()

# Now let's check the residual scatter plots by Species
ggplot(data = ModelOutput, mapping = aes(x = Predictions, y = Residuals)) +
  geom_point(aes(color = Species)) + 
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~ Species, nrow = 3) +
  labs(title = "Residual Scatter Plots by Species") +
  theme_light()

#####################################################################
# Modeling Non-Linear Relationships
# Read in the mpg data set

setwd("C:/Users/hamin/Desktop/CMSC462/CourseDataSets")
mpg <- read_tsv("mpg.txt")

# We would like to predict hwy with the displ variable
# First let's plot the data
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(color = "#006EA1") +
  labs(title = "Highway Fuel Efficiency vs Displacement",
       y = "MPG Highway",
       x = "Vehicle Displacement") + 
  theme_light()

# First let try a simple linear regression
SimpleLR = lm(hwy ~ displ, data = mpg)
summary(SimpleLR)

# Let's view the residual scatter plot
SimpleLROutput = data.frame(hwy = mpg$hwy,
                            displ = mpg$displ,
                            Residuals = SimpleLR$residuals,
                            Predictions = SimpleLR$fitted.values)

# Scatter plot, notice a curve?
ggplot(data = SimpleLROutput, mapping = aes(x = Predictions, y = Residuals)) +
  geom_point(color = "#006EA1") + 
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Scatter Plot for Simple Linear Fit") +
  theme_light()

# Original data with simple regression line
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(color = "#006EA1") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Highway Fuel Efficiency vs Displacement",
       y = "MPG Highway",
       x = "Vehicle Displacement") + 
  theme_light()

# Now lets add a displ^2 term to account for the non-linearity
# Please note that the I() functions is known as As is function, so 
QuadraticFit = lm(hwy ~ displ + I(displ^2), data = mpg)

# View the summary output
summary(QuadraticFit)

# Now let's make a scatter with the quadratic fit
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(color = "#006EA1") +
  geom_line(aes(x = displ, y = QuadraticFit$fitted.values), color = "red") +
  labs(title = "Highway Fuel Efficiency vs Displacement",
       y = "MPG Highway",
       x = "Vehicle Displacement") + 
  theme_light()

