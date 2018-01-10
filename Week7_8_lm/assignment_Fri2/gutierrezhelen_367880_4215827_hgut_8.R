setwd("C:/Users/helen/Documents/winter_2017/538_stats/lab/W7")

data("airquality")
head(airquality)

mod.oz = lm(Temp~Ozone, data=airquality) 
summary(mod.oz)

#EXERCISE 4: adjusted R2 
#R2 = 1 - ((RSS/ n-k)/ (SSY/n-1)) --> k= number of parameters; n = samples size (calc with length (x))
res.oz = residuals(mod.oz)
RSS.oz = sum(res.oz^2)
k = 2
n = length (res.oz)

#Remove N/A variables
data.not.NA = subset(airquality, Ozone != "NA")
SSY.oz = deviance(lm(Temp~1, data=data.not.NA))
#SSY.oz = 10347.06
SSY1.oz = var(data.not.NA$Temp)*115
#SSY1.oz = 10347.06

#adjusted R2 = 1 - ((RSS/(n-k))/(SSY/(n-1)))
adjR2 = 1 - (RSS.oz/(n-k))/(SSY.oz/(n-1))
adjR2
#adjR2 = 0.4832134

#EXERCISE 5:
  #Residual Independency
plot(residuals(mod.oz))
library(lmtest)
dwtest(mod.oz, alternative=c("two.sided"))
##DW = 1.1833; p-value = 7.276e-06
# This indicates that there is some degree of autocorrelation within the residuals.

#homoscedasticity
plot(residuals(mod.oz)~fitted(mod.oz))
abline(lm(residuals(mod.oz)~fitted(mod.oz)), col="red")
library(lmtest)
bptest(mod.oz)
## BP = 3.2346, df = 1, p-value = 0.0721
#The test indicates that the residuals are homoscedastic

#normality
qqnorm(residuals(mod.oz))
qqline(residuals(mod.oz), col="red")
shapiro.test(residuals(mod.oz))
## W = 0.94867, p-value = 0.000226
#The test indicates that the residuals are normally distributed.