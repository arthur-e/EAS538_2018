##1

data("airquality")
head(airquality, 15)

pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])

#Creating a correlation table:

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
 
cor(x, y1)
cor(x, y2)

plot(x, y1)
plot(x, y2)

########Exercise 1

data("faithful")
head(faithful, 15)

pairs(faithful[, c("eruptions", "waiting")])

cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")

#Result: correlation coefficient = 0.9008112

##2

#Wind = x, Temp = y

mod = lm(Temp~Wind, data=airquality)
summary(mod)

plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")


####Exercise 2

mod = lm(Temp~Ozone, data=airquality)
summary(mod)

plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")

#For each 1 unit increase of ozone, the estimated temperature increases by roughly 0.2 degrees
#Both the intercept and coefficient for Ozone (slope) are significant at <2e-16 ***
#The R-squared value is 0.4832 

mod = lm(eruptions~waiting, data=faithful)
summary(mod)

plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")

#For each 1 unit increase in waiting time, the estimated length of eruption increases by 0.0756 units
#Both the intercept and coeffiient for Ozone (slope) are significant at <2e-16 ***
#The R-squared value is 0.8108


####Exercise 3

res = residuals(mod)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod)$df[2])

RMSE = RSS/153
RMSE

MSE = RSS/summary(mod)$df[2]
MSE

anova(mod)
anova(mod)

#Manually calculated MSE = 46.5, confirmed by the anova test




