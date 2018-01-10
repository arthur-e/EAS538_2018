#Exercise: Calculate the adjust R2R2 manually. With the Ozone model
data("airquality")
AQ2=airquality[which(is.na(airquality$Ozone)==FALSE)]
mod1 = lm(Temp~Ozone, data=airquality)
summary(mod1)
res = residuals(mod1)
RSS = (sum(res^2))
SSY = deviance(lm(airquality$Ozone~1))

### Oscar: 
### Don't you think having R^2 this high is unexpected? Making a plot will help you judge your calculation.
### This is the Sum of Square of "Y" (SSY). Y is the response variable, temperature, not Ozone!

R2=1-RSS/SSY #0.9573622

AdjR2 = 1 - (RSS/(151))/(SSY/152)
# Adusted R2 = 0.9573622

# The adjusted R^2 indicates that the model explains
#more variability of the response data around its mean 
#than would be explained by chance

#Exercise 5

#Independency
plot(residuals(mod1))
dwtest(mod1, alternative=c("two.sided"))
# residuals show a temporal autocorrelation (p=7.276e-06)

#Homoscedasticity

library(lmtest)
bptest(mod1)

#test indicated that the data is not homeoscedastic (but is close) (p=0.0721)

#Normality

qqnorm(residuals(mod1))
qqline(residuals(mod1),col="red")
shapiro.test(residuals(mod1))

#data are normally distributed (p=0.000226)

### Oscar: 
### No, it does NOT pass the Shapiro test. If you plot the qqplot of the residuals, you will find that it does not look normal. 
