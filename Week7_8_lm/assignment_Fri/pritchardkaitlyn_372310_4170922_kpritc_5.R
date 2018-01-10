## EXERCISE 1
data('faithful')
pairs(faithful[, c("eruptions", "waiting")])
cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")
plot(eruptions~waiting, data=faithful)
#Since R is 0.90, there is a very strong, positive correlation between the waiting time btw. eruptions and the duration of the eruption

## EXERCISE 2 #1
data("airquality")
head(airquality, 15)
mod=lm(Ozone~Wind, data=airquality)
summary(mod)
plot(Ozone~Wind, data=airquality)
abline(lm(Ozone~Wind, data=airquality), col="blue")
# There is negative correlation between ozone and wind speed
# For every unit increase in wind speed, there's a 5.5 unit decrease in ozone
# R^2 = 0.36, so 36% of the total variability in ozone can be explained by wind speed

## EXERCISE 2 #2
data('faithful')
head(faithful, 15)
mod2=lm(eruptions~waiting, data=faithful)
summary(mod2)
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful))
# For every unit increase in waiting time, there's a 0.08 unit increase in eruption duration
# R^2=0.81, so 81% of the total variability in eruption duration is explained by waiting time

## EXERCISE 3
res=residuals(mod)
RSS=sum(res^2)
RSS #79859.01
RSE=sqrt(RSS/summary(mod)$df[2])
RSE #=26.47
RMSE=RSS/153
RMSE #=521.95
MSE=RSS/summary(mod)$df[2]
MSE #=700.52

## EXERCISE 4
#Calculating R^2
SSY=deviance(lm(airquality$Ozone~1))
SSY #=125,143
SSY1=var(airquality$Ozone)*152
R2=1-RSS/SSY
R2 #0.36
#Calculating adjusted R^2
numerator=RSS/(2-1)
numerator
r2adj=1-(numerator/SSY)
r2adj #=0.36

## EXERCISE 5  need residual independeny, homosed, normaity
plot(residuals(mod))
install.packages("lmtest")
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
#Since d<2, there is evidence of positive serial correlation.
#Since p<0.05, we reject the null hypothesis that there is no correlation among residuals 
# We should manipulate the data before doing the linear regression
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red")
bptest(mod)
#Sice p<0.05, we reject the null hypothesis that the variance of residuals is constant
# This means that heteroscedasticity is present and we should transform the data before doing a linear regression
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
shapiro.test(residuals(mod))
#Since p<0.05, we reject the null hypothesis that our samples came from a normal distribution