##Exercise 1##

data("faithful")
head(faithful,15)

cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")

plot(faithful$waiting, faithful$eruptions)
fit<-lm(faithful$eruptions~faithful$waiting)
abline(fit)

#Since the R-value is 0.9008, the waiting time between eruptions and the duration of the eruptions are positively correlated, meaning that the longer the wait, the longer the eruption is.


###################################################

##Exercise 2##

#part 1
mod1=lm(Temp~Ozone, data=airquality)
summary(mod1)

plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")

#part 2
mod2=lm(eruptions~waiting, data=faithful)
summary(mod2)

plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")


#######################################################

##Exercise 3##

mod1=lm(Temp~Ozone, data=airquality)
summary(mod1)

res = residuals(mod1)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod1)$df[2])

RMSE = RSS/153
RMSE

MSE = RSS/summary(mod1)$df[2]
MSE

anova(mod1)

#calculating manually matches the anova output, which is 46.5


##################################################

##Exercise 4##  I know that the adjusted R2 should be 0.2045, so I just have to make sure it works!

mod=lm(Temp~Wind, data=airquality)
summary(mod)

res = residuals(mod)
RSS = sum(res^2)
r=RSS/(153-2)
r

SSY = deviance(lm(airquality$Temp~1))
s = SSY/(153-1)
s

AdjustedR2= 1-(r/s)
AdjustedR2

#The adjusted R2 value is 0.2045, so it worked!


#####################################################

## Exercise 5##

plot(residuals(mod1))

#Test for residual independency#
library(lmtest)
dwtest(mod1, alternative=c("two.sided"))
#The p-value is very close to zero, therefore we know that our residuals are showing a temporal autocorrelation and are not independent, which means we should manipulate the data before doing a linear regression

#Test for residual homoscedasticity#

#check to see if there is any structured pattern in the variance of the residuals
plot(residuals(mod1)~fitted(mod1))
abline(lm(residuals(mod1)~fitted(mod1)), col="red")
#the variance appears to be distributed evenly throughout the plot, so it looks like constant variance ("homoscedasticity")
#can also check using the lmtest

library(lmtest)
bptest(mod1)

#The p-value is slightly over 0.05, but we are relatively confident that they are homoscedastic (and it's better than the temperature and wind residuals)

#Test for residual normality#
qqnorm(residuals(mod1))
qqline(residuals(mod1), col="red")
#from the QQ pot, it looks like they are normally distributed, but let's also check with the shapiro test

shapiro.test(residuals(mod1))
#the p-value is 0.00023, which implies that the residuals are normally distributed, whcih is what we want.

##This inspection is essentially the same as the one for temperature and wind.  If we were to correct for the temporal autocorrelation issue, then this linear model would perform well. 
