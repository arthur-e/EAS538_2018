data("faithful")
head(faithful, 15)
pairs(faithful[, c("eruptions", "waiting")])
cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")
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
head(y1,20)
head(x,20)
head(y2,20)
cor(x,y1)                                           
cor(x,y2)
plot(y1~x)
modely1=lm(y1~x)
summary(modely1)
abline(reg=modely1)
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful),col="red")

# Exercise 2

# Question 1

data("airquality")
head(airquality, 15)
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp", "Month","Day")])
cor(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], use="na.or.complete")
mod=(lm(Temp~Ozone,data=airquality))
plot (Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="blue")

#Question 2

pairs(faithful[, c("eruptions", "waiting")])
cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")
f = function(x){
  y=-(x+3)^2 + runif(length(x), min=-4, max=4)
  #print(y)
}
f1 = function(x){
  y=runif(length(x), min=min(x), max=max(x))
}

x=seq(from=-8, to=0, by =0.005)
y1=f(x)
y2=f1(x)
head(y1,20)
head(x,20)
head(y2,20)
cor(x,y1)                                           
cor(x,y2)
plot(y1~x)
modely1=lm(y1~x)
summary(modely1)
abline(reg=modely1)
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful),col="red")


#Question 3

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
head(y1,20)
head(y2,20)
head(x,20)
cor(x,y1)
cor(x,y2)
plot(y1,x)
plot(y2,.x)
plot(x, y1)
plot(x,y2)
abline(x~y1, data=airquality)
model1=lm(y2~x)
summary(model1)
abline(reg=model1)
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality),col="red")
res=residuals(model1)
RSS=sum(res^2) 
RSE=sqrt(RSS/summary(model1)$df[2])
RMSE=RSS/153
RMSE
mod=(lm(Temp~Ozone,data=airquality))
res=residuals(mRMSE=RSS/153)
RSS=sum(res^2)
RSE=sqrt(RSS/summary(mod)$df[2])
RMSE=RSS/153
RMSE
MSE=RSS/summary(mod)$df[2]
MSE
str(mod)
anova(mod)
#Exercise 4
SSY=deviance(lm(airquality$Ozone~1))
SSY1=var(airquality$Ozone)*152
R2=1-RSS/SSY
SSY
#Exercise 5
plot(residuals(mod))
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
# Durbin-Watson test
#data:  mod
# DW = 1.1833, p-value = 7.276e-06
# alternative hypothesis: true autocorrelation is not 0
# The Durbin-Watson test is not 0 and shows there's positive serial correlation.

plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red")
plot(mod, which=c(1))

bptest(mod)
# The Breusch-Pagan test determines that heteroscedasticity is present and if it is NOT, residuals are homoscedastic.
# 	studentized Breusch-Pagan test
# data:  mod
# BP = 3.2346, df = 1, p-value = 0.0721
# In this case, the p-value is barely greater than .05 showing that the residuals are homoscedastic.
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
plot(mod,which=c(2))
shapiro.test(residuals(mod))

#Shapiro-Wilk normality test
# data:  residuals(mod)
# W = 0.94867, p-value = 0.000226

# The qqnorm plot didn't show a perfectly fit line for residuals. 
#The Shapiro-Wilk normality test has a p-value less than .05 indicating the residuals are not normally distributed.
plot(mod)

