### Exercise 4 ###

mod.ex4 = lm(Temp~Ozone, data=airquality)
res.ex4 = residuals(mod.ex4)
RSS.ex4 = sum(res.ex4^2)
k = 2
n = length(res.ex4)

dat.new = subset(airquality, Ozone!="NA")
ssy.ex4 = deviance(lm(Temp~1, data=dat.new))
adjr2 = 1-(RSS.ex4/(n-k))/(ssy.ex4/(n-1))
adjr2

# R^2 = 0.48321

summary(mod.ex4)
 
### Exercise 5 ###

## Independence of residuals 
plot(residuals(mod.ex4))

install.packages(lmtest)
library(lmtest)
dwtest(mod.ex4, alternative=c("two.sided"))

# The residuals show a clear temporal autocorrelation (p-value = 7.276e-06)

## Homoscedasticity
install.packages(lmtest)
library(lmtest)
bptest(mod.ex4)

# The residuals are homoscedastic (p-value = 0.0721)

## Normally distributed residuals

shapiro.test(residuals(mod.ex4))

# Residuals are not normally distributed (p-value = 0.000226)








