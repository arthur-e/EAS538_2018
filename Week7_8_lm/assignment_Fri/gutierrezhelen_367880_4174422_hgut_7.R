setwd("C:/Users/helen/Documents/winter_2017/538_stats/lab/W7")

###ASSIGNMENTS: EX. .1-3
data("faithful")
head(faithful)

data("airquality")
head(airquality)

#EXERCISE 1:
plot(faithful$waiting, faithful$eruptions)
cor(faithful[,c("eruptions", "waiting")])
##the correlation coefficient between eruptions and waiting time is 0.9 which means
##that there is a strong positive correlation between the two.

#EXERCISE 2:
#2.1
mod.air = lm(Temp~Ozone, data=airquality) 
summary(mod.air)

plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")
#The R-squared value is 0.49, which means that there is almsost a 50% probability 
#that the observed values match the expected values. The plot indicates that there
#are some outliers which has an effect on the correlation values.

#2.2
mod.faithful = lm(waiting~eruptions, data=faithful) 
summary(mod.faithful)
#the R-squared value is 0.81 which means that there is an 81% probabilitythat the 
#observed values match the expected values. The linear model tells us that for 
#every minute waited, the eruption time will be 10.73 minutes.

#EXERCISE 3: 
res = residuals(mod.faithful)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod.faithful)$df[1])
##RSE = 68.715

MSE = RSS/summary(mod.faithful)$df[1]
##MSE = 4721.694

#EXERCISE 4:
SSY = deviance(lm(faithful~1))
SSY1 = var(airquality$Temp)*152
R2 = 1 - RSS/SSY

#EXERCISE 5:
#Residual Independency
plot(residuals(mod.air))
library(lmtest)
dwtest(mod.air, alternative=c("two.sided"))

#homoscedasticity
plot(residuals(mod.air)~fitted(mod.air))
abline(lm(residuals(mod.air)~fitted(mod.air)), col="red")
library(lmtest)
bptest(mod.air)

#normality
qqnorm(residuals(mod.air))
qqline(residuals(mod.air), col="red")
shapiro.test(residuals(mod))
