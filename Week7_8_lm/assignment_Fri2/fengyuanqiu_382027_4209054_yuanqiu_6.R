

##EXERCISE 3
## Calculate the residual standard error (RSE) and mean square error (MSE) manually for the model you built 
## last time.

res = residuals(mod2)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod2)$df[2])

RMSE = RSS/153
RMSE
#RMSE: 34.645

MSE = RSS/summary(mod2)$df[2]
MSE
#MSE: 46.497

anova(mod2) #MSE: 46.5


## EXERCISE 4
## Calculate the adjust R^2 manually.

mod.ex4 = lm(Temp~Ozone, data=airquality)
res.ex4 = residuals(mod.ex4)
RSS.ex4 = sum(res.ex4^2)

k = 2

n = length(res.ex4)

ex4.new = subset(airquality, Ozone!="NA"&Temp!="NA")

SSY.ex4= deviance(lm(Temp~1, data=ex4.new))
SSY1.ex4 = var(ex4.new$Temp)*115

adjR2 = 1 - (RSS.ex4/(n-k))/(SSY1.ex4/(n-1))

adjR2


## EXERCISE 5
## Check residual independency, homoscedasticity, and normality for the model with ozone as the independent 
## variable and interpret the results briefly.

plot(residuals(mod2)~fitted(mod2))
abline(lm(residuals(mod2)~fitted(mod2)), col="red")

dwtest(mod2, alternative=c("two.sided"))
## p-value = 7.276e-06
## Temporal data shows significant temporal autocorrelation. Data needs to be manipulated to adjust for seasonal
## and diurnal effects.

bptest(mod2)
## p-value = 0.0721
## Data is homoscedastic (p>0.05), but just barely.

qqnorm(residuals(mod2))
qqline(residuals(mod2), col="red")
shapiro.test(residuals(mod2))
## p-value = 0.000226
## This data does not appear to be normally distributed. 