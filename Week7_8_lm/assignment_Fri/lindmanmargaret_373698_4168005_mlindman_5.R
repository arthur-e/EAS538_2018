#EX1
data("faithful")

plot(faithful[, c("eruptions", "waiting")])
cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")


#EX2
mod = lm(Temp~Ozone, data=airquality)
plot(Temp~Ozone, data=airquality, pch=20, col="orange")
abline(lm(Temp~Ozone, data=airquality), col="purple")

mod_2 = lm(eruptions~waiting, data=faithful)
plot(eruptions~waiting, data=faithful, pch=20, col="turquoise")
abline(lm(eruptions~waiting, data=faithful), col="forestgreen")

#EX 3
res = residuals(mod)
summary(mod)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod)$df[2])
MSE = RSS/summary(mod)$df[2]


