data("airquality")
AirQualMod = lm(Temp~Ozone, data=airquality)

###EXERCISE 4: Calculate the adjusted R2 manually
airqualnona=airquality[which(is.na(airquality$Ozone)==FALSE),]
newmod = lm(Temp~Ozone, data=airqualnona)

res2 = residuals(newmod)
RSS2 = sum(res2^2)
numerator = RSS2/(length(airqualnona$Ozone)-2)
denomenator = var(airqualnona$Temp)
numerator
denomenator
adjustedR2= 1-(numerator/denomenator)
adjustedR2
summary(newmod)
#adjustedR2=0.4832 which checks out based on the summary table

###EXERCISE 5

plot(residuals(AirQualMod))
#test independency with dwtest
install.packages(lmtest)
library(lmtest)
dwtest(AirQualMod, alternative=c("two.sided"))
plot(residuals(AirQualMod)~fitted(AirQualMod))
abline(lm(residuals(AirQualMod)~fitted(AirQualMod)), col="red")
#both plot and p value show that there is a autocorrelation, 
#this means that the variables are not independent of each other

#test homoscedasticity with bptest
install.packages(lmtest)
bptest(AirQualMod)
#The bp test gives us a p-value of 0.0721
#This means that the residuals are not homoscedastic

#test for normality with qqnorm
qqnorm(residuals(AirQualMod))
qqline(residuals(AirQualMod), col="red")
shapiro.test(residuals(AirQualMod))
#p-value = 0.000226
#this means that the residuals are not normally distributed


##With these three tests, we see that these three assumptions are violated 
##and we should not use a linear model!