###Lab 07
data("airquality")#extra stuff
head(airquality, 15)
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])
cor(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], use="na.or.complete")
###Exercise 1
data("faithful")#read in data
head(faithful)
plot(faithful[, c("eruptions", "waiting")])#plot of eruptions vs. waiting time
cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")#correlation coefficients between eruptions and waiting time
#r=0.9008
###
###Exercise 2, Part 1
new_mod = lm(Ozone~Solar.R, data=airquality)#New linear model, ozone vs. solar radiation
summary(new_mod)
plot(Ozone~Solar.R, data=airquality)#plot of model
abline(lm(Ozone~Solar.R, data=airquality), col="red")#regression line
###Exercise 2, Part 2
Faithful_mod = lm(eruptions~waiting, data=faithful)#Faithful linear model, waiting time vs. eruption duration
summary(Faithful_mod)
plot(eruptions~waiting, data=faithful)#plot of model
abline(lm(eruptions~waiting, data=faithful), col="red")#regression line
###
###Exercise 3
res = residuals(new_mod)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(new_mod)$df[2])
RSE
MSE = RSS/summary(new_mod)$df[2]
MSE
#RSE=31.33457; MSE=981.8553
###