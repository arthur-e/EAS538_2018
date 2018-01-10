#Exercise 4####
data("airquality")
head(airquality)
aa=subset(airquality,is.na(Ozone)==FALSE&is.na(Temp)==FALSE,data=airquality)
mod1=lm(Temp~Ozone,data=aa)
res1=residuals(mod1)
RSS=sum(res1^2)
n=length(res1)
k=2
SSY1=var(aa$Temp)*115
R2=1-(RSS/(n-k))/(SSY1/(n-1))
R2 #adjusted R square=0.4832134

SSY=deviance(lm(aa$Temp~1))
R2_1=1-(RSS/(n-k))/(SSY/(n-1))
R2_1 #R2_1=0.4832134

#Exercise 5####
mod2=lm(Temp~Ozone,data=airquality)
plot(residuals(mod2))
install.packages("lmtest")
library(lmtest)
dwtest(mod2,alternative=c("two.sided"))
#p-value = 7.276e-06, mod2 has significant autoforrelation

bptest(mod2)
#p-value=0.0721, mod2 is homoscedastic

qqnorm(residuals(mod2))
qqline(residuals(mod2),col="red")
shapiro.test(residuals(mod2))
#p-value=0.000226,so mod2 is not normaly distributed
