#Exercise 1
data(faithful)
pairs(faithful[, c("eruptions", "waiting")])
cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")
#r=.9008, therefore eruption time and waiting time are strongly positively correlated

#Exercise 2
data("airquality")
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")
mod.ozone = lm(Temp~Ozone, data=airquality)
summary(mod.ozone) 
#intercept = 69.4, p-value = 2e-16
#ozone regression coefficient (slope)=.2, p-value = 2e-16
#r^2=.49, therefore ozone and temperature are positively correlated and about half of the variation in Temperature can be explained by the linear relationship between Temperature and Ozone

plot(eruptions~waiting, data=faithful)
mod.faithful = lm(eruptions~waiting, data=faithful)
summary(mod.faithful)
#intercept==1.87, p-value = 2e-16
#slope=.07, p-value = 2e-16
#r^2=.81, where waiting time and eruption time are positively correlated (you see a longer eruption if you wait longer) and about 81% of the variation in eruption time can be explained by the linear relationship between eruption time and waiting time

#Exercise 3
res = residuals(mod.ozone)
RSS = sum(res^2) 
RSE = sqrt(RSS/summary(mod.ozone)$df[2]) #6.82
MSE = RSS/summary(mod.ozone)$df[2] #46.5 
anova(mod.ozone) #for confirmation

