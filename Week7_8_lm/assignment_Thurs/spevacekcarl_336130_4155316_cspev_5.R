##Exercise 1
data("faithful")
plot(faithful$eruptions, faithful$waiting)
cor(faithful$eruptions, faithful$waiting)
# r = .9008112, so as one variable increases one unit, there is a 90% chance the other variable will increase one unit
# There appears to be a positive linear relationship between the two variables.

##Exercise 2
data("airquality")
mod1 = lm(Temp~Ozone, data = airquality)
summary(mod1)
#Intercept = 69.41072
#Ozone Coefficient = 0.20081
#The p-values for both are less than 0.05 (2e-16), so both coefficients are significant, which means they are significantly different from 0.
#As ozone increases one unit, temperature increases 0.20081 units.
#R-squared = 0.4877, so the model explaines 48.77% of the variation in Temperature.
plot(Temp~Ozone, data = airquality)
abline(mod1, col = "red")

mod2 = lm(eruptions~waiting, data = faithful)
summary(mod2)
#Intercept = -1.874016
#Waiting coefficient = 0.075628
#The p-values for both are less than 0.05 (2e-16), so both coefficients are significant, which means they are significantly different from 0.
#As wiating time increases by one unit, eruption time increases by 0.075628 units.
#R-squared = 0.8115, so the model explains 81.15% of the variation in eruption time.
plot(eruptions~waiting, data = faithful)
abline(mod2, col = "red")

#Exercize 3
res1 = residuals(mod1)
RSS1 = sum(res1^2)
RSE1 = sqrt(RSS1/summary(mod1)$df[2])
RSE1
MSE1 = RSS1/summary(mod1)$df[2]
MSE1
