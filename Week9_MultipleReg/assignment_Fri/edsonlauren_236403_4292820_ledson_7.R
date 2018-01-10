##Lauren Edson
## NRE 538 Lab 7

data("airquality")
head(airquality, 15)

library(dplyr)
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

pairs(airquality[,c(1:4)])
cor(airquality[,c(1:4)], use="na.or.complete")


# Exercise 1
# Manipulate the above code to reduce the correlation between X1X1 and X2X2 
# and recalculate the regression coefficients. Explain what you find.
# hint: Increase the random noise of X1X1 and X2X2 so that the two can be less correlated.

Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=8)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=8)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df)) #coefficient = 1.37
summary(lm(Y~X2, data=df)) #coefficient = 1.23
summary(lm(Y~X1+X2, data=df)) #coefficient = 0.99
plot(X1~X2)
cor(X1, X2) #correlation = 0.4589443
#Since I allowed for a greater amount of random noise in X1 and X2, the 
#relationship between Y and X1 and X2, becomes less correlated. In the plot,
#this looks like a more scattered plot, rather than a linear relationship.
#The correlation value is now 0.458, as opposed to 0.96, as it was with smaller noise.


#########################################################
# Exercise 2
# [Bonus, 2 pts] Challenge yourself by calculating the RSS of the two models, 
# FF statistics (1 pt) and the p-value (1 pt) all by yourself (i.e. not using the 
#  anova() function)

mod1 = lm(Temp~Wind, data=airquality)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod1) #degrees of freedom is 109 
summary(mod2) #degrees of freedom is 108
#anova(mod1, mod2) #F value is 8.3739

res1 = residuals(mod1)
RSS1 = sum(res1^2) 
RSS1 #7520.672

res2 = residuals(mod2)
RSS2 = sum(res2^2) 
RSS2 #6979.506

Fstat= ((RSS1-RSS2)/(3-2))/(RSS2/(108)) 
Fstat #8.373933

pf(Fstat,109,108) #1

# Compare a third model (mod3) with ozone varaible as the third independent 
#variable to mod1 and mod2 above. Does it explain more variance? Does it give 
#you reasonable estimate of regression coefficients?

mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod1, mod2, mod3)
# Analysis of Variance Table
# 
# Model 1: Temp ~ Wind
# Model 2: Temp ~ Wind + Solar.R
# Model 3: Temp ~ Wind + Solar.R + Ozone
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1    109 7520.7                                  
# 2    108 6979.5  1    541.17 11.589 0.0009349 ***
# 3    107 4996.6  1   1982.90 42.463 2.424e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##The RSS of mod3 is significantly lower than both mod1 and mod2. The probability of
## mod2 to explain the same amount of variance as mod 3 is 2.4*10^-9
## The mod3 does explain more variance than the other models and gives reasonable 
## numbers for the regression coefficients. It tells us that ozone is the most significant 
##explanatory variable


#########################################################

# Exercise 3
# Compare the AIC of the three models above mod1, mod2, and mod3. 
# Do you have different conclusion in terms of which model performs better?

AIC(mod1) #788.9671
AIC(mod2) #782.6779
AIC(mod3) #747.5795

##With AIC, we are looking for the lowest value of AIC, or for a difference 
## of 4 to 7 between models. In this case, mod3 is still the model that 
## explains the most wihtout being penalized for having too many variables,
## since it has the lowest AIC value, and a difference of greater than 10 implies
## that it is significant. 
