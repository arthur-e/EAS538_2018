###Exercise 1
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=10, max=100)
X2 = (Y+3)/2 + runif(length(Y), min=0, max=1)
df = as.data.frame(cbind(Y, X1, X2))
plot(X1~X2)
cor(X1,X2) #r=.064 (although it varies each time due to nature of function)
summary(lm(Y~X1, data=df)) #X1= .013
summary(lm(Y~X2, data=df)) #X2= 1.97
#because I introduced more noise into the model, X1 and X2 are less likely to approach the values of 3 and 2 respectively based on a purely linear model with no noise where X1 and X2 are highly correlated. 
#Instead, the noise in the equation makes the variables less likely to achieve these values by chance and therefore reduces correlation of the variables, which is good for our model.
#The plot above shows a random scatter of points and no clear relationship between the variables, as described by the r value

###Exercise 2
data("airquality")
library(dplyr)
head(airquality, 15)
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod1, mod2, mod3) 
#yes, adding ozone in mod3 helps to explain more variability in the model, with a significantly lower RSS value than the other models (~5000) indicated by a very small p value, and large F statistic. 
#This model includes the nested models of mod1 and mod2, which make for clear comparison between them

# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1    109 7520.7                                  
#2    108 6979.5  1    541.17 11.589 0.0009349 ***
#3    107 4996.6  1   1982.90 42.463 2.424e-09 ***

###Exercise 3
AIC(mod1, mod2, mod3) 
#     df      AIC
#mod1  3 788.9671
#mod2  4 782.6779
#mod3  5 747.5795
#mod3 shows a large difference between mod1 and mod2 due to a difference of about 35-40
#This confirms the results in Exercise 2 that mod3 is the best model of the three options.
