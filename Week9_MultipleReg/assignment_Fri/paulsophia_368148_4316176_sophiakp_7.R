##Excercise 1
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=60)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))

summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))

plot(X1,X2)
cor(X1,X2)
##Increasing the max value increases the possible distribution, creating less correlation with X2

##Excercise 2
data("airquality")
library(dplyr)
air.1 = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
mod1 = lm(Temp~Wind, data=air.1)
mod2 = lm(Temp~Wind+Solar.R, data=air.1)
mod3= lm(Temp~Wind+Solar.R+Ozone, data=air.1)

anova(mod1, mod2,mod3)
##Comparing all three makes it appear that mod2 and mod3 both explain more of the variation
##than model one but mod3 is even better
anova(mod2, mod3)
##Comparing mod2 and mod3 reveals that mod3 is significantly better at explaining variance in temperature

##Excercise 3
AIC(mod1,mod2,mod3)
##this suggests that the models are fairly comparable, but mod1 has a slightly better fit
