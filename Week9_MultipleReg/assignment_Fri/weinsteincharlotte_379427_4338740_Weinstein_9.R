###Lab 9
###Charlotte Weinstein

###Exercise 1

#define variables and relationships between them (original example):
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)

#calculate correlation between X1 and X2:
cor(X1, X2)
#cor = 0.97
#This original correlation is very high

#increase the amount of noise by increasing the maximum value in the runif function:
X1a = (Y-2)/3 + runif(length(Y), min=0.5, max=10)
X2a = (Y+3)/2 + runif(length(Y), min=0.5, max=10)
cor(X1a, X2a)
#when max=10 for X1a, cor is reduced to .51
#if max=10 for X2a as well, then cor is further reduced to .37

X1b = (Y-2)/10 + runif(length(Y), min=0.5, max=2)
X2b = (Y+3)/12 + runif(length(Y), min=0.5, max=2)
cor(X1b, X2b)
#increasing the slope also reduces cor (cor=.62)

X1c = (Y-20)/3 + runif(length(Y), min=0.5, max=2)
X2c = (Y-15)/2 + runif(length(Y), min=0.5, max=2)
cor(X1c, X2c)
#changing the intercept, however, does not appear to change the correlation between X1c and X2c
#cor=0.97

##### Oscar: Yes, chaging the relationship between Xs and Y will not change the correlation between Xs. 
#####        You can reduce the correlation between Xs simply by increasing the noise (runif part).
#####        However, doing this will also make your estimation of regresion coefficient off

###Exercise 2

data("airquality") #load airquality data
library(dplyr) #allows us to use %>%
airquality1 = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
#subset airquality data to remove NA values

mod1 = lm(Temp~Wind, data=airquality1) #model 1
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality1) #model 2
summary(mod2)
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality1) #model 3 with Ozone added
summary(mod3)

anova(mod1, mod2, mod3)
#The probability that mod3 explains the the same amount of variance as mod2 is very small,
#indicating that mod3 explains a significantly greater amount of variance.
#The regression coefficients also do not differ from those calculated by mod1 and mod2
#to an abnormal degree or change sign, which suggests that they are reasonable.

##### Oscar: In the f-test output, model3 is actually being compared to model 1, NOT model 2.
#####        Whether a regression coefficient is reasonable depends on whether this varaible is correlated with others.


###Exercise 3

AIC(mod1, mod2, mod3, k=2) #calculate AIC for each model
#mod3 has the smallest AIC in the output (747.58), suggesting that it has the best fit.
#mod2 has the second smallest AIC (782.68), suggesting that it is the next best fit.
#mod1 has the largest AIC (788.97), suggesting that it is the worst fit out of the three models.
#This is consistent with what we observed from the comparisons in Exercise 2.
