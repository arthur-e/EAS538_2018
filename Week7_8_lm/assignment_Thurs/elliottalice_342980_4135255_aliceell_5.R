##Exercise 1
pairs(faithful[, c('eruptions', 'waiting')])
cor1 <- cor(faithful$eruptions, faithful$waiting)

##Exercise 2
#1. 
plot(Temp~Ozone, data=airquality)
mod = lm(Temp~Ozone, data=airquality)
summary(mod)
abline(mod, col="red")
#2.
summary(lm(eruptions~waiting, data=faithful))

##Exercise 3
res = residuals(mod)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod)$df[2])
RMSE = RSS/153
MSE = RSS/summary(mod)$df[2]

RSE
MSE