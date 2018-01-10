setwd("D:/UM/SNRE/Winter 2017/NRE 538 Statistics/Week 8 Lab")
data("airquality")

#Exercise 4 Calculate the adjust R2 manually
airquality1=subset(airquality,select=c(Ozone,Temp))
airquality2=na.exclude(airquality1)
modair=lm(formula=Temp~Ozone,data=airquality2)
summary(modair) #The model is the same with the one using "airquality" dataset directly
res=residuals(modair)
RSS=sum(res^2)
SSY = deviance(lm(airquality2$Temp~1))
R2=1-((RSS/114)/(SSY/115))
R2
#The adjusted R2=0.4832134 (Confirmed by summary(modair))

#Exercise 5 Check residual independency, homoscedasticity, and normality for the model with ozone as the independent variable and interpret the results briefly.
#Residual Independency
plot(res)
library(lmtest)
dwtest(modair, alternative=c("two.sided"))
#the p-value for Aurbin-Watson test< 0.05. The residuals are not independent but autocorrelated.

#Homoscedasticity
plot(res~fitted(modair))
abline(lm(res~fitted(modair)),col="red")
plot(modair, which=c(1))
bptest(modair)
#The p-value for Breusch-Pagan test >0.05. The residuals are homoscedastic.

#Normality of residuals
qqnorm(res)
qqline(res,col="red")
plot(modair, which=c(2))
shapiro.test(res)
#The p-value for shapiro test <0.05. The residuals are not normally distributed