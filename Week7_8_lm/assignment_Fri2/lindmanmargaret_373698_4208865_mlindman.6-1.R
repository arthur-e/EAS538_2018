#EX 4
mod.4 = lm(Temp~Ozone, data=airquality)
res.mod4 <-residuals(mod.4)
RSS.4 = sum(res.mod4^2)
k = 2
n = length(res.mod4)
dat.new<-subset(airquality, Ozone!="NA")
SSY.4 = deviance(lm(Temp~1, data=dat.new))
adjR2 = 1 - (RSS.4/(n-k))/(SSY.4/(n-1)) 


#EX 5
install.packages("lmtest")
library(lmtest)
plot(mod.4, which=c(2))
plot(residuals(mod.4)~fitted(mod.4))
abline(lm(residuals(mod.4)~fitted(mod.4)), col="red")
dwtest(mod.4, alternative=c("two.sided"))
#p-value is less than 0.05, which means that we can reject the null hypothesis that there is no strucutre in our residuals (by random chance). 
bptest(mod.4)
#p-value is greater than 0.05, therefore our residuals have constant variance (homoscedastic) which is good :)
shapiro.test(residuals(mod.4))
#p-value is less than 0.05, which means that our residuals deviate from a normal distribution, which means that there is something that our model is not explaining.