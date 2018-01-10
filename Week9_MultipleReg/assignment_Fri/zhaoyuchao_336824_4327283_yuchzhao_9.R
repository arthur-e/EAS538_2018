data("airquality")
head(airquality, 15)
library(dplyr)
airquality = airquality %>% 
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

###Exercise 1
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=4)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)
plot(X1~X2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
###Coefficients:
###Estimate Std. Error t value Pr(>|t|)    
###X1 2.34525    0.09095  25.787  < 2e-16 ***
summary(lm(Y~X2, data=df))
###Coefficients:
###Estimate Std. Error t value Pr(>|t|)    
###X2 1.97893    0.02041   96.97   <2e-16 ***
summary(lm(Y~X1+X2, data=df))
###Coefficients:
###Estimate Std. Error t value Pr(>|t|)    
###X1 0.17588    0.05413   3.249  0.00136 ** 
###X2 1.86441    0.04049  46.042  < 2e-16 ***

###The conclusion is that after increasing the noise of X1, it's coefficient value in lm(Y~X1) is decrease compare with the samll noise it used to be. There is no dbout in the lm(Y~X1+X2) X1's coefficient value is way more off. But from the plot it can be seen, by increase the noise of X1, the corelation between X1 and X2 is reduced. 
####Therefore, in the lm(Y~X1+X2) modele, the coffecient value of X2 is less off (estimate better) than beofre we reduce the corelation of X1 and x2 by incresing the noise of X1.



###Exercise 2
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod1, mod2, mod3)

###Model 1: Temp ~ Wind
###Model 2: Temp ~ Wind + Solar.R
###Model 3: Temp ~ Wind + Solar.R + Ozone
###Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
###1    109 7520.7                                  
###2    108 6979.5  1    541.17 11.589 0.0009349 ***
###3    107 4996.6  1   1982.90 42.463 2.424e-09 ***

###From the output table, we see that RSS of mod3 is 4996.6. 
###The interpretation of these results is that mod3 explains significantly greater amount of varaince comparing to mod2, because the probability for mod2 to explain the same amount of variance as mod3 is 2.424e-09.


### Exercise 3
AIC(mod1) ###788.9671
AIC(mod2) ###782.6779
AIC(mod3) ###747.5795
### the smaller the AIC is the less information is lossed, therefore, mod3 is a better performed model compare with other two models.