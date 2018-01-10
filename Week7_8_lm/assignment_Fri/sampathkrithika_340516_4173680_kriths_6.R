data("faithful")
faithful

###Exercise 1
# plot and calculate correlation coefficients B/w waiting time between eruptions and duration of the eruption

pairs(faithful[, c("eruptions", "waiting")])
cor(faithful[, c("eruptions", "waiting")])

###Exercise 2
#estimated regression line
mod= lm(Temp~Ozone, data= airquality)
summary(mod)
plot(Temp~Ozone, data= airquality)
abline(lm(Temp~Ozone, data= airquality), col="red")

mod1= lm(eruptions~waiting, data=faithful)
summary(mod1)
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful))

###Exercise 3
# RSE and MSE
res= residuals(mod)
RSS= sum(res^2)
RSE= sqrt(RSS/summary(mod)$df[2])
RSE
MSE= RSS/summary(mod)$df[2]

###Exercise 4
SSY=deviance(lm(airquality$Ozone~1))
SSY1= var(airquality$Ozone)*152
R2= 1- RSS/SSY
R2

###Exercise 5
#residual independency, homoscedasticity, normality
plot(residuals(mod))
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red")
bptest(mod)
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
shapiro.test(residuals(mod))

# auto-correlation: the residuals show a clear temporal autocorrelation (residuals are not independent)
# homoscedasticity: the variances are evenly distributed- the residuals are homoscedastic
# normality: the QQ plot and the Shapiro- Wilk test show that the residuals are normally distributed
