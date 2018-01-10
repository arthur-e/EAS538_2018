##Excercise 1
data("faithful")
head(faithful)
plot(faithful$waiting,faithful$eruptions)
cor(faithful$waiting,faithful$eruptions)
##there is a correlation between waiting time and eruption duration

##Excercise 2
data("airquality")
head(airquality)
mod1 = lm(Temp~Ozone, data=airquality)
summary(mod1)
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")
##Temperature and ozone are positively related
mod2=lm(eruptions~waiting, data=faithful)
summary(mod2)
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")
##yes, if you have to wait longer you are likely to see a larger eruption
##a lot of the variance in eruption length is explained by wait time

##Excercise 3
res1 = residuals(mod1)
RSS1 = sum(res1^2)
RSE1 = sqrt(RSS1/summary(mod1)$df)
MSE1 = RSS1/summary(mod1)$df
