###Lab 8
###Exercise 4
mod.ex4 = lm(Temp~Ozone, data = airquality)#linear model
res.mod4 = residuals(mod.ex4)#Finding residuals
RSS4 = sum(res.mod4^2)#Finding RSS of residuals
k = 2
n = length(res.mod4)
data_new = subset(airquality, Ozone!="NA")#Finding SSY with deviance method
SSY4 = deviance(lm(Temp~1, data = data_new))
SSY1_4 = var(data_new$Temp)*115#Finding SSY with variance method
adjR2 = 1 - (RSS4/(n-k))/(SSY4/(n-1))#Can use SSY4 or SSY1_4
adjR2#Value is 0.4832
summary(mod.ex4)#Adjusted R2 matches maual method.
###
###Exercise 5
new_mod = lm(Temp~Ozone, data=airquality)#New linear model, temperature vs. ozone 
summary(new_mod)
plot(residuals(new_mod))
library(lmtest)
dwtest(new_mod, alternative=c("two.sided"))#p-value<0.000001: there is a pattern, or autocorrelation in our residuals, meaning no independency.
plot(residuals(new_mod)~fitted(new_mod))
abline(lm(residuals(new_mod)~fitted(new_mod)), col="blue")
library(lmtest)
bptest(new_mod)#p-value=0.0721: there is homoscedasticity in the data.
qqnorm(residuals(new_mod))
qqline(residuals(new_mod), col="blue")
shapiro.test(residuals(new_mod))#p-value=0.000226: the test shows that the data are not normally distributed, and the qq plot emphasizes this.
plot(new_mod)
#The results of our tests suggest that the data displays homoscedasticity which is great, 
#but also displays autocorrelation, and is not normally distributed. Therefore, a linear model using ozone
#as an indepedent variable may not be the best option.
###