### Exercise 1 ###

Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=15)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=15)
df = as.data.frame(cbind(Y, X1, X2))

cor(X1, X2)
# With increased random noise, X1 and X2 are less highly correlated
# Correlation coefficient = 0.1276961, which is less than the 0.5 rule of thumb
# X1 and X2 are not too correlated so both can be included in the model

summary(lm(Y~X1, data=df))
# regression coefficient of Y on X1 = 0.47017
# R^2 = 0.1412
summary(lm(Y~X2, data=df))
# regression coefficient of Y on X2 = 0.69626
# R^2 = 0.3816
summary(lm(Y~X1+X2, data=df))
# regression coefficient of Y on X1 = 0.37762 (close to true value of 0.47017)
# regression coefficient of Y on X2 = 0.65282 (close to true value of 0.69626)
# R^2 = 0.4712

# The regression coefficients for both X1 and X2 are still close to the true values when both X1 and X2 are included in the model
# This is because X1 and X2 are not highly correlated
# Furthermore, the R^2 increased because more variables are included in the model

### Exercise 2 ###

data("airquality")
library(dplyr)
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

# Calculating the RSS
mod1 = lm(Temp~Wind, data=airquality)
res1 = residuals(mod1)
RSS1 = sum((res1)^2)
RSS1
# RSS for Model 1 = 7520.672
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
res2 = residuals(mod2)
RSS2 = sum((res2)^2)
RSS2
# RSS for Model 2 = 6979.506

# Calculating the F Statistic
F-statistic = ((RSS1-RSS2)/(2-1))/(RSS2/(110-2))
F-statistic
# F-statistic = 8.373933

# Calculating p-value
p-value = pf(q=8.373933, df1=1, df2=108, lower.tail=FALSE)
p-value
# p-value = 0.004605628

# Does Model 3 explain more variance?
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
# Model 3 R^2 = 0.4999
# Model 2 R^2 = 0.3014
# Model 1 R^2 = 0.2472
anova(mod1, mod2, mod3)
# p-value = 2.424e-09, thus the differences in R^2 are significant 
# Model 3 does explain more variance than Models 1 and 2 

# Is Model 3 a reasonable estimate of variance?
vif.rad = 1/(1 - summary(lm(Temp~Wind+Solar.R+Ozone, dat=airquality))$r.squared)
vif.rad
# vif.rad = 1.999406, which is less than 3 so the model is a reasonable estimate of regression coefficients

### Exercise 3 ###
AIC(mod1)
# AIC score = 788.9671
AIC(mod2)
# AIC score = 782.6779
AIC(mod3)
# AIC score = 747.5795
# Model 3 is the best because it has the lowest AIC score


