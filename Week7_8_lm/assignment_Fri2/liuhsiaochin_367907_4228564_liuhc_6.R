data("airquality")
head(airquality)

#Exercise 4
ex4=subset(airquality,is.na(Ozone)==FALSE&is.na(Temp)==FALSE,data=airquality)
mod.ex4=lm(Temp~Ozone,data=ex4)
res=residuals((mod.ex4))
RSS=sum(res^2)
SSY=deviance(lm(ex4$Temp~1))
n=116
R2.ex4=1-((RSS/(n-2)))/(SSY/(n-1))
R2.ex4 #adjusted r square = 0.4832134

#Exercise 5
##Residual Independency Test
plot(residuals(mod.ex4)) #
install.packages("lmtest")
library(lmtest)
dwtest(mod.ex4,alternative = c("two.sided"))
#P-value=7.276e-06,which means the residuals have a temporal autocorrelation

#Residual Homoscedasticity Test
plot(residuals(mod.ex4)~fitted(mod.ex4))
abline(lm(residuals(mod.ex4)~fitted(mod.ex4)),col="red")
bptest(mod.ex4)
#P-value=0.0721, which means the residuals homoscedastic

#Residual Normality Test
qqnorm(residuals(mod.ex4))
qqline(residuals(mod.ex4),col="red")
shapiro.test(residuals(mod.ex4))
#p-value=0.000226,which means the residuals is not normaly distributed

