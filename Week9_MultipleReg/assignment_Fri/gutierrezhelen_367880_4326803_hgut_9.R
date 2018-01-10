setwd("C:/Users/helen/Documents/winter_2017/538_stats/lab/W9")
data("airquality")
head(airquality, 15)

library(dplyr)
airquality = airquality %>% #(%>% cleans up the data --> create data subset to eliminate na)
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

pairs(airquality[,c(1:4)])

#EXERCISE 1:
#Answer: by increasing the noise (increasing the length of Y to greater than 10 
#units), you reduce the correlation between X1 and X2, and vice versa (correlation
#increases with less noise).
Y = seq(from=0, to=20, by=0.1)
X.1 = (Y-2)/3 + runif(length(Y), min=0, max=10)
X.2 = (Y+3)/2 + runif(length(Y), min=0, max=10)
df = as.data.frame(cbind(Y, X.1, X.2))

summary(lm(Y~X.1, data=df))
summary(lm(Y~X.2, data=df))
summary(lm(Y~X.1+X.2, data=df))
plot(X.1~X.2)
cor(X.1, X.2)

y = seq(from=0, to=20, by=0.1)
x.1 = (Y-2)/3 + runif(length(Y), min=0, max=15)
x.2 = (Y+3)/2 + runif(length(Y), min=0, max=15)
df = as.data.frame(cbind(y, x.1, x.2))

summary(lm(y~x.1, data=df))
summary(lm(y~x.2, data=df))
summary(lm(y~x.1+x.2, data=df))
plot(x.1~x.2)
cor(x.1, x.2)

#EXERCISE 2:
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod1, mod2, mod3)

#RESULTS:
#By adding Ozone to the model, we can see that the regression coefficients for both 
#wind and solar radiation lower considerably, and that the effect of wind and solar
#radiation on temperature is no longer significant. The adjusted R-squared value of
#model 3 is 0.49, which is higher than models 1 & 2, and means that model 3 has greater 
#explanatory power than models 1 & 2. The variance test (f-test) tells us that the 
#RSS value for model 3 is 4996.6, which accounts for significantly (p-value of 2.24e-09.)
#more variance than models 1 & 2.

#EXERCISE 3:
mod.sel = AIC (mod1, mod2, mod3)
#          df         AIC
#mod1       3         788.9671
#mod2       4         782.6779
#mod3       5         747.5795

#RESULTS:
#model 3 has a smaller AIC value compared to the other models (difference between 
#AIC value for mod 3 and AIC values for mod 1 and mod 2 is significant). This means
#that model 3 performs better than both models 1 & 2. Also, model 2 seems to perform
#better than model 1 (difference in AIC values between mod 1 and mod2 is between 
#4 and 7).