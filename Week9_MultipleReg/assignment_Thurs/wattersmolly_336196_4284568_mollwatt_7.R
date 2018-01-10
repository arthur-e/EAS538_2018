data("airquality")
head(airquality, 15)

library(dplyr)

#get rid of the N/A rows
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)


#-----------------------------------
###EXERCISE 1
##original highly correlated model
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))

plot(X1~X2)
cor(X1, X2)

##new manipulated model with increased noise
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=10)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=10)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))

plot(X1~X2)
cor(X1, X2)
#increasing the random noise means that the data is less consistent and therefore less correlated
#the coefficient estimates are more different from the true values with more noise than they are with less noise

###EXERCISE 2
##F-test 
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
#only wind as x

mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)
#wind and solar radiation as x varible

#compare two models
anova(mod1, mod2)

##compare third model to first two
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)

anova(mod1, mod2, mod3)
#mod3 does explain more variance because the F is increased and the RSS is decreased.
#the p value is low which shows that mod3 is significantly different from mod2
#the coefficients change when you add ozone and are no longer significant or have a significant effect, but the signs and magnitude are relatively similar so they seem reasonable.

##bonus question: calculating RSS of mod1 and mod2 manually
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)

RSS1=sum(residuals(mod1)^2)
RSS1
RSS2=sum(residuals(mod2)^2)
RSS2

F=((RSS1-RSS2)/(3-2))/(RSS2/(length(airquality$Temp)-3))
F
pvalue = 1-pf(F,1,(length(airquality$Temp)-3))
pvalue


###EXERCISE 3
#compare AIC of 3 models (basically same as exercise 2 but with AIC)
AIC(mod1)
#AIC=788.9671
AIC(mod2)
#AIC=782.6779
AIC(mod3)
#AIC=747.5795
##We have the same conclusion as we had in exercise 2 that mod3 performs best
##This is because the AIC is smallest

























