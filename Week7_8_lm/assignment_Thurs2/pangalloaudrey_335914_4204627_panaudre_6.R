#lab 6

SSY = deviance(lm(airquality$Temp~1))
SSY1 = var(airquality$Temp)*152
R2 = 1 - RSS/SSY
SSY
SSY1
R2
#Exercise 4
res = residuals(mod)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod)$df[2])
r1= RSS/(153-2)
r2= SSY/(153-1)
radjust= r1/r2
#0.795 is the answer I got
#Onward
F = ((SSY-RSS)/1)/MSE
1-pf(F, df1=1, df2=summary(mod)$df[2])
?pf
plot(residuals(mod))
install.packages('lmtest')
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="magenta")
library(lmtest)
bptest(mod)
qqnorm(residuals(mod))
qqline(residuals(mod), col="orange")
plot(mod, which=c(2))
plot(mod)

#Exercise 5
library(lmtest)
dwtest(funstuff, alternative=c("two.sided"))
# DW= 1.18, p value= 7.276e-06
plot(residuals(funstuff)~fitted(funstuff))
abline(lm(residuals(funstuff)~fitted(funstuff)), col="red")
library(lmtest)
bptest(funstuff)
#BP= 3.2346, p=0.0721
shapiro.test(residuals(funstuff))
# W= 0.94, p value= 0.000226
plot(funstuff)
# my flu brain was having trouble remaking something for just ozone
#so I just used the model I made before
#So the Durbin-Watson stat did show temporal autocorrelation, I think
#The plot for homoscedasticity looks like it MIGHT be ok. I'm not totally sure though
#I have no idea what to make of the line on my last graph. it looks nothing like yours.
