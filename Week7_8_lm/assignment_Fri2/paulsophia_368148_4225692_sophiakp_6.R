##Excercise 4
mod4=lm(Temp~Ozone, data=airquality)
res4=residuals(mod4)
RSS4=sum(res4^2)
k=2
n=length(res4)
sub4=subset(airquality, Ozone!="NA")
SSY4=deviance(lm(Temp~1, data=sub4))
##or
SSY4a=varience(airquality$Ozone)*115
adjr2=((RSS4/(n-k))/(SSY4/(n-1)))
##or
adjr2=((RSS4/(n-k))/(SSY4a/(n-1)))
##Excercise 5
plot(residuals(mod4))
#there appears to be a pattern
library(lmtest)
dwtest(mod4, alternative=c("two.sided"))
#the pvalue is very small so it seems that the residuals are autocorrelated which makes because this is data over time

#Homoscadasticity test
plot(residuals(mod4)~fitted(mod4))
abline(lm(residuals(mod4)~fitted(mod4)), col="red")
##it looks like there is a pattern in the varience of the residuals
library(lmtest)
bptest(mod4)
#The p value is about 0.07 so there is a fair chance that there is a pattern

qqnorm(residuals(mod4))
qqline(residuals(mod4), col="red")
shapiro.test(residuals(mod4))
#the residuals are not normally distributed