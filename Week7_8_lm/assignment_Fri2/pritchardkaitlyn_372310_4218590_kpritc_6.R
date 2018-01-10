## EXERCISE 4
data("airquality")
mod=lm(Temp~Ozone, data=airquality)
summary(mod)
res=residuals(mod)
RSS=sum(res^2)
k = 2
n = length(res)

newdata=subset(airquality, Ozone!="NA")
newdata
#!= means 'not equal to'
SSY=deviance(lm(Temp~1, data=newdata))
SSY1 = var(newdata$Temp)*115
SSY
SSY1
adjr2= 1- (RSS/(n-k))/(SSY1/(n-1))
adjr2 ## = 0.4832134
summary(mod) #Adjusted R2 = 0.4832

## EXERCISE 5  need residual independeny, homosced, normality
plot(residuals(mod))
install.packages("lmtest")
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
#Since d<2, there is evidence of positive serial correlation.
#Since p<0.05, we reject the null hypothesis that there is no correlation among residuals 
# We should manipulate the data before doing the linear regression
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red")
bptest(mod)
#Sice p<0.05, we reject the null hypothesis that the variance of residuals is constant
# This means that heteroscedasticity is present and we should transform the data before doing a linear regression
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
shapiro.test(residuals(mod))
#Since p<0.05, we reject the null hypothesis that our samples came from a normal distribution