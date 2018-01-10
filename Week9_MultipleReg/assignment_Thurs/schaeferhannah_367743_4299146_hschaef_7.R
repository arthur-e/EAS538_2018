data("airquality")
head(airquality, 15)
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
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


## Exercise 1 chaange the noise or the slope, esp the noise
## this should help change the correlation
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=2, max=100)
X2 = (Y+3)/2 + runif(length(Y), min=2, max=100)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
plot(X1~X2)
## As the maximum values for the graph increase, in this case to
## 100, the coefficients for X1 and X2 gradually get smaller.
## When the maximum and minimum values are farther apart, X1
## and X2 will be less correlated. The correlation coefficient 
## for X2 also became negative. The correlation coefficients are
## no longer close to their true values because X1 = 0.004 and
## X2 = -0.02.

y=seq(from=1, to=20, by=0.1)
y=runif(length(y), min=3, max=5)
x2=x1*2+5
x2=x1*2+5
df = data.frame(y, x1, x2)
pairs(df)
cor(df)
lm(y~x1+x2, data=df)
summary(lm(y~x1+x2, data=df))

mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)
anova(mod1, mod2)


## Exercise 2
## mod1= lm(Temp~Wind, data=airquality)
## mod2= lm(Temp~Wind+Solar.R, data=airquality)
## mod3= lm(Temp~Wind+Solar+Ozone, data=airquality)
mod3= lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod1, mod2, mod3)

## Yes it explains more variance because mod2 and mod3 are
## both very significant compared to mod1 according to the
## anova. When we ran the anova with only mod1 and mod2, the
## p value was higher than the p values when mod3 was included.

## Exercise 3
AIC(mod1, mod2, mod3, k = 2)
## I would still say mod3 performs the best because it has the 
## lowest AIC value. The AIC value for mod3 is 747.6, and the
## AIC for mod2 is 782.7, and the AIC for mod1 is 789.0. The lower
## AIC value indicated less information is lost through the 
## model.


mod.r = lm(Temp~Solar.R, data=airquality)
mod.wr = lm(Wind~Solar.R, data=airquality)
summary(lm(residuals(mod.r)~residuals(mod.wr)))
summary(lm(residuals(mod.r)~Wind, data=airquality))







