data("airquality")
str(airquality)

head(airquality, 15)
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])
cor(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], use="na.or.complete")

set.seed(15165)
f = function(x){
y=-(x+3)^2 + runif(length(x), min=-2, max=2)
#print(y)
}
f1 = function(x){
y=runif(length(x), min=min(x), max=max(x))
}
x=seq(from=-6, to=0, by =0.005)
y1=f(x)
y2=f1(x)
head(x, 20)
head(y1, 20)
head(y2, 20)
cor(y1, x)
cor(y2, x) 
plot(x, y1) ## y is response variable, x is independent vairable
plot(x, y2)

plot(y1~x) ## gives same results "y depends on x"
plot(y2~x) ## gives same results

mod = lm(Temp~Wind, data=airquality)
summary(mod)
str(summary(mod)) ##can use this to find df and where df is in the line

plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")


res = residuals(mod)
RSS = sum(res^2) ##Residual sum of squares
RSE = sqrt(RSS/summary(mod)$df[2]) ##Residual standard error
RMSE = RSS/153 ##Root mean square error
MSE = RSS/summary(mod)$df[2] ##Mean square error
anova(mod)


plot(residuals(airqualitymodel))

install.packages("lmtest")
library(lmtest)
dwtest(mod, alternative=c("two.sided"))


is.na(airquality)
which(is.na(airquality$Ozone)==FALSE)
aa = subset(airquality, is.na(Ozone)==FALSE)
aa

## Exercise 4
## airqualitymodel Ozone
SSYOzone = deviance(lm(airquality$Ozone~1))
R2adjust = 1 - RSS/SSYOzone
R2adjust = 0.914
## This R2 value is very high and suggests 91% of the variance is explained by the model.

### Oscar: 
### Don't you think having R^2 this high is unexpected? Making a plot will help you judge your calculation.
### This should be the Sum of Square of "Y" (SSY). Y is the response variable, temperature, not Ozone!


## Exercise 5
library(lmtest)
dwtest(lm(Temp~Ozone, data=airquality), alternative=c("two.sided"))
## DW = 1.1833, p-value = 7.276e-06, alt hypothesis: true autocorrelcation is not 0.
bptest(lm(Temp~Ozone, data=airquality))
## BP= 3.2346, df= 1, p-value= 0.0721
shapiro.test(residuals(airqualitymodel))
qqnorm(residuals(airqualitymodel))
qqline(residuals(airqualitymodel), col="red")
## W = 0.94867, p-value = 0.000226
## The data are normally distributed.
plot(airqualitymodel)



