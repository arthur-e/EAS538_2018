data("airquality")

#Exercise4
modnew1=lm(Temp~Ozone, data=airquality)
summary(modnew1)
res = residuals(modnew1)
RSS = sum(res^2)
k = 2
n=length(res)

newdata = subset(airquality, Ozone!="NA")

SSY = deviance(lm(Temp~1, data=newdata))
SSY1 = var(newdata$Temp)*115
adjR2 = 1 - (RSS/(n-k))/(SSY/(n-1))
R2 = 1 - (RSS)/(SSY)
R2#0.4877
SSY
adjR2 #0.4832134
 

#Exercise 5
#install.packages(lmtest)
library(lmtest)
dwtest(modnew1, alternative=c("two.sided"))

plot(residuals(modnew1)~fitted(modnew1))
abline(lm(residuals(modnew1)~fitted(modnew1)), col="red")
# p-value = 7.276e-06 thus the residuals show temporal autocorrelation.


#install.packages(lmtest)
library(lmtest)
bptest(modnew1)
#p-value = 0.0721, since our p-value is not less than 0.05 residuals are homoscedastic

qqnorm(residuals(modnew1))
qqline(residuals(modnew1), col="red")
shapiro.test(residuals(modnew1))
#From the QQ plot and shapiro-wilk test, the residuals are not normally distributed
#p-value = 0.000226