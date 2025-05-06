# This file shows some simple R commands

library(tidyverse)
library(ggfortify)
#shows how to use different probability functions in R

############Normal distribution##############################
#pdf for Normal not probability
mySeq = seq(-6, 6, 0.001)

p = ggdistribution(dnorm, mySeq, mean = 0, sd = 1, colour = "blue")
p = ggdistribution(dnorm, mySeq, mean = 0, sd = 2, colour = "green",p=p)
ggdistribution(dnorm, mySeq, mean = 0, sd = 3, colour = "red",p=p)
#This shows cumulative probability 
ggdistribution(pnorm, seq(-10, 30, 0.01), mean = 10, sd = 5)

###########################################################
#######################Poisson Distribution#################

N = 100 
x_array = seq(0,N,1)


poisson_density = dpois(x_array, 5)
poisson_cumul = ppois(x_array, 5)
poisson_density  
poisson_cumul  
#Plot PDF of Poisson which is same as probability
ggdistribution(dpois, x_array, lambda = 50)
############################################################
binom_density = dbinom(x_array,N, .5)
binom_cumul = pbinom(x_array, N, .5)
binom_density  
binom_cumul  
#Plot PDF of Binomial which is same as probability
ggdistribution(dbinom, x_array, size = N, prob=.5)

pbinom(5, 10, .5, FALSE)


randomUniformNumber = runif(1000, 50, 150)


########################################################
#####################################################
#Binning in R
library(dplyr)
myNumber = rnorm(1000, mean=75, sd=25)


new_bin = cut(myNumber, breaks=c(min(myNumber), 50, 60, 70, 80, 90, max(myNumber)))

