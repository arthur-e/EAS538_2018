##Exercise 4##
data("airquality")
is.na(airquality$Ozone)
which(is.na(airquality$Ozone)==FALSE)
airquality2 = airquality[which(is.na(airquality$Ozone)==FALSE),]
mod1 = lm(Temp~Ozone, data= airquality2)
res = residuals(mod1)
RSS = sum(res^2)

SSY = deviance(lm(airquality2$Temp~1))
R2 = 1 - (RSS/(115-2))/(SSY/(115-1))
R2
##Adjusted R2 = 0.4831736

##Exercise 5##
install.packages(lmtest)
library(lmtest)

#Residual Independency
dwtest(mod1, alternative=c("two.sided"))
#The residuals are autocorrelated (since the p-value is significant)

#Homoscedasticity
bptest(mod1)
#Residual is not homoscedastic (large p-value)

### Oscar: the null hypothesis here is that the residuals are homoskedastic, so high p-value means we can not reject the null hypothesis. 


##Normality
qqnorm(residuals(mod1))
qqline(residuals(mod1), col="red")
#Residuals do follow normal distribution

### Oscar: No, the residuals in the middle part deviate from the theoretical line. It does not look like normal distriution. 