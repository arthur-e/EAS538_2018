# Assignment #6
# Christopher Askew-Merwin
# 2/23/2017

data("airquality")

aq = airquality[which(is.na(airquality$Ozone)==FALSE),]

mod1 = lm(Temp~Ozone, data=aq)
summary(mod1)

res = residuals(mod1)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod1)$df[2])
RSE # = [1] 6.818914

RMSE = RSS/153
RMSE # = [1] 34.64526

MSE = RSS/summary(mod1)$df[2]
MSE # = [1] 46.49759

#######Exercise 4

#Calculate the adjust R2 manually.

SSY = deviance(lm(aq$Ozone~1)) 

### Oscar: 
### Don't you think having R^2 this high is unexpected? Making a plot will help you judge your calculation.
### This is the Sum of Square of "Y" (SSY). Y is the response variable, temperature, not Ozone!


SSY1 = var(aq$Ozone)*152
R2 = 1 - RSS/SSY
SSY

R2.a = 1 - (RSS/(151)/(SSY/152))
R2.a
# [1] 0.9573622

#######Exercise 5

#Check residual independency, homoscedasticity, and 
#normality for the model with ozone as the 
#independent variable and interpret the results 
#briefly.

dwtest(mod1, alternative=c("two.sided"))
#Durbin-Watson test

#data:  mod1
#DW = 1.1833, p-value = 7.276e-06
#alternative hypothesis: true true autocorrelation is not 0
# the residuals are autocorrelated

bptest(mod1)
#studentized Breusch-Pagan test

#data:  mod1
#BP = 3.2346, df = 1, p-value = 0.0721
# the residuals are homeoscedastic

qqnorm(residuals(mod1))
qqline(residuals(mod1), col="red")
# the residuals are normally distributed

### Oscar's comment: 
### No, it does NOT pass the Shapiro test. If you plot the qqplot of the residuals, you will find that it does not look normal. 

