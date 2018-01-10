####Exercise #4
data("airquality")
airquality1=subset(airquality, is.na(Ozone)==FALSE)
mod.ozone = lm(Temp~Ozone, data=airquality1)
res = residuals(mod.ozone)
RSS = sum(res^2) 
SSY = deviance(lm(airquality1$Temp~1))
R2 = 1 - RSS/SSY
Adjusted_R2 = 1 - (RSS/(115-2))/(SSY/(115-1))
Adjusted_R2 #manual calculation = .48317
summary(mod.ozone) #r calculation = .4832

####Exercise 5
plot(residuals(mod.ozone))
#independence
library(lmtest)
dwtest(mod.ozone, alternative=c("two.sided")) #is autocorrelated, p=7.3e-06

#homoscedasticity
plot(residuals(mod.ozone)~fitted(mod.ozone))
abline(lm(residuals(mod.ozone)~fitted(mod.ozone)), col="red")
bptest(mod.ozone) #not homoscedastic, p=.07

#normality
qqnorm(residuals(mod.ozone))
qqline(residuals(mod.ozone), col="red") #looks close to normal with some straying points
shapiro.test(residuals(mod.ozone)) #confirm residuals are not normal where p=.0002

