library(tidyverse)
library(readxl)

# Import dataset
setwd("C:/Users/hamin/Desktop/CMSC462/A3")
covid_data <- read_excel("CovidMortality.xlsx")

# Let's see the distribution of Deaths by Poverty Rate
ggplot(data = covid_data, mapping = aes(x = Deaths)) +
  geom_histogram(aes(fill = PovertyRate), color = "white", bins = 15) +
  facet_wrap(~PovertyRate, nrow = 2) +
  labs(title = "Distribution of Deaths by Poverty Rate",
       x = "Deaths", y = "Number of States") +  theme_light()

# Let's see the relationship between Deaths and Population
ggplot(data = covid_data, mapping = aes(y = Deaths, x = Population)) +
  geom_point(aes(color = PovertyRate), size = 2) +
  labs(title = "Deaths by Population Colored by Poverty Rate",
       x = "Population",
       y = "Deaths") +
  theme_light()

# Add linear regression lines by Poverty Rate, by default this includes interaction terms
# since the slopes can be different for each category
# However in our example, the assumption of equal slopes seems reasonable
ggplot(data = covid_data, mapping = aes(y = Deaths, x = Population)) +
  geom_point(aes(color = PovertyRate), size = 2)  +
  geom_smooth(aes(color = PovertyRate), method = "lm", se = FALSE) +
  labs(title = "Deaths by Population Colored by Poverty Rate",
       x = "Population",
       y = "Deaths") +
  theme_light()
class(covid_data$PovertyRate)

# Now we fit a multiple linear regression that predicts Deaths
# by PovertyRate and Population
#CovidMortalityModel = lm(Deaths ~ Population + Area + Confirmed, data = covid_data)
CovidMortalityModel = lm(Deaths ~ Confirmed + Area, data = covid_data)
CovidMortalityModel = lm(Deaths ~ Confirmed + Area + PovertyRate, data = covid_data)


# Let's see the summary output
summary(CovidMortalityModel)