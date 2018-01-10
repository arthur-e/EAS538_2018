### Exercise 4
mod = lm(Temp~Ozone, data=airquality)
summary(mod)  ### 114df and 37 observations are NA and Adjusted R-squared:  0.4832
air=airquality[which(is.na(airquality$Ozone)==FALSE),]
   ### or air= subset(airquality, is.na(Ozone)==FALSE)
SSY1=var(air$Temp)*114
res = residuals(mod)
RSS = sum(res^2)
R2_adjust = 1 - RSS/SSY1
R2_adjust  ### 0.4832143


### Exercise 5
### Independant of residual 
mod = lm(Temp~Ozone, data=airquality)
plot(residuals(mod))
install.packages("lmtest")
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
     ###Durbin-Watson test
        data:  mod
        DW = 1.1833, p-value = 7.276e-06
        alternative hypothesis: true autocorrelation is not 0
### The residuals show a clear temporal autocorrelation. This means we should manipulate the data before doing the linear regression.

 
bptest(mod)
     ###studentized Breusch-Pagan test
         data:  mod
         BP = 3.2346, df = 1, p-value = 0.0721
### The results show that the residuals are homoscedastic, which is good.


shapiro.test(residuals(mod))
     ###Shapiro-Wilk normality test
        data:  residuals(mod)
        W = 0.94867, p-value = 0.000226
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
 ###From the QQ plot and the shapiro-wilk test, we can say that the residuals are not normally distributed.