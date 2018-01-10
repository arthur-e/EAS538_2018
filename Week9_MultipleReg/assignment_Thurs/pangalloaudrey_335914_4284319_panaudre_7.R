data("airquality")
head(airquality, 15)
library(dplyr)
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
pairs(airquality)
cor(airquality)
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))    
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))
plot(X1~X2)
cor(X1, X2)
#Exercise 1
Y2 = seq(from=0, to=20, by=0.1)
X3 = (Y2-20)/7 + runif(length(Y), min=0.5, max=2)
X4 = (Y2+750)/20 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y2, X3, X4))
summary(lm(Y2~X3, data=df))    
summary(lm(Y2~X4, data=df))
summary(lm(Y2~X3+X4, data=df))
plot(X3~X4)
cor(X3, X4)
# cor- 0.457- I know my numbers are kind of crazy. I wanted a crazy change in the correlation
#so I just kept increasing numbers until I got what I wanted. 
# the lm output still looks vaguely normally distributed, I think, 1Q and 3Q are mostly the same
# max and min appear to be close too, so they float in the right place in regards to the mean
# p value is still very significant. I suppose with this model, the fact that they are less correlated elminates that bias you mentioned earlier
# With this model, although my numbers are ridiculous, shows that we can now determine which variable is actually contributing to whatever hypothesis we are testing here, if I understand this correctly
#part 2
y=seq(from=1, to=20, by=0.1)
y=runif(length(y), min=3, max=5)
x1=seq(from=5, to=24, by=0.1)
x2=x1*2+5
df = data.frame(y, x1, x2)
pairs(df)
cor(df)
vif.rad = 1/(1 - summary(lm(Solar.R~Wind+Ozone, dat=airquality))$r.squared)
vif.rad
#part 3
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)
anova(mod1, mod2)
#Exercise 3
mod3= lm(Temp~Wind+Solar.R+Ozone, data = airquality)
summary(mod3)
plot(mod3)
#This model got worse somehow. The p values for Wind and Solar.R are no longer significant (0.1, 0.3 respectively)
#And while two coefficients seem reasonable, the Solar.R does not. 
#However the adjusted R squared indicates that it does explain more variance, because it's higher
# EDIT: You answered this question, the lack of significance is becacuse ozone has a stronger effect on the model compared to the other two
#so this model does explain the variance
#Part 4
AIC(mod1, k=2)
AIC(mod2, k=2)
AIC(mod3,k=2)
# So, smallest AIC is the best model, according to this, mod3 is the best model because it has the smallest AIC (747.57)
#This makes sense considering the results of the regression 
#Part 5
mod.r = lm(Temp~Solar.R, data=airquality)
mod.wr = lm(Wind~Solar.R, data=airquality)
summary(lm(residuals(mod.r)~residuals(mod.wr)))
summary(lm(residuals(mod.r)~Wind, data=airquality))
