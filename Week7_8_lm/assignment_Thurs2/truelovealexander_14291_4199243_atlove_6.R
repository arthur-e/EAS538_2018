###Exercise 4

data("airquality")

airquality[which(is.na(airquality$Ozone)==FALSE),]

mod = lm(Temp~Ozone, data=airquality)
summary(mod)

res = residuals(mod)
RSS = sum(res^2)

SSY = deviance(lm(airquality$Temp~1))
SSY1 = var(airquality$Temp)*152
R2 = 1 - RSS/SSY
SSY
SSY1
R2

###Exercise 5

mod = lm(Temp~Ozone, data=airquality)
summary(mod)

#Residual Independency
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
#p-value = 7.276e-06, data is autocorrelated

#Residual Homoscedacicity
library(lmtest)
bptest(mod)
#p-value = 0.0721, >0.05 data is nearly homoscedastistic, slightly heteroscedastistic


#Residual Normality
shapiro.test(residuals(mod))
#p-value = 0.000226, data is normal

plot(mod)
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
