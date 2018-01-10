data("airquality")
library(dplyr)
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

###Exercise 1
##First Fake data set
Y = seq(from = 0, to = 20, by = 0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data = df))
summary(lm(Y~X1+X2, data=df))
plot(X1, X2)
cor(X1, X2)

###Second fake data set, attempting to make X12 and X22 non-colinear
Y1 = seq(from = 0, to = 20, by = 0.1)
X12 = (Y-2)/3 + runif(length(Y), min=0.5, max=20)
X22 = (Y+3)/2 + runif(length(Y), min=0.5, max=20)
df1 = as.data.frame(cbind(Y1, X12, X22))
summary(lm(Y1~X12+X22, data=df1))
plot(X22~X12)
cor(X12, X22)
##By increasing noise, we affected the accuracy of the coefficient estimate.
##The two variables are not highly correlated, but the random noise overwhelmed the true relationship between X and Y from the main equation.


##Exercise 2
RSS1 = sum(residuals(mod1)^2)
RSS2 = sum(residuals(mod2)^2)
F = ((RSS1-RSS2)/(3-2))/(RSS2/(length(airquality$Temp)-3))
F
p = 1 - pf(F, 1,(length(airquality$Temp)-3))
p

##Model comparison
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)
anova(mod1, mod2)
##Model 1 has has a higher degree of freedom of residuals and residual sum of squares.
##F-test tells us that when we added one degree of freedom, the residual sum of squares droped significantly.
##Model 2 explains more variation in Y.
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod1, mod2, mod3)
##Model 3 does explain more variance because the residual sum of squares decreased and the F-statistic incresed.
##The p-value shows that model 3 is significantly different from model 2.
##The coefficients for wind and solar are no longer significant using model 3, but the signs and effect sizes are not drastically different.

##Exercise 3
AIC(mod1)
AIC(mod2)
AIC(mod3)
##The AIC for model 2 is only 6 units smaller than model 1, so model 1 and model are not that different in terms of AIC score.
##Model 3 has the lowest AIC value. The AIC for model 3 is about 35 units lower than the AIC for model 2 and 41 units lower than the AIC for model 1, so it does explain more variance.
