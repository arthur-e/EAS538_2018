# Exercise1
data("faithful")
head(faithful)
cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")
# The correlation coefficient between the waiting time between eruptions and the duration of the eruption for the Old Faithful geyser in Yellowstone National Park is 0.9008112 

plot(eruptions~waiting,data=faithful)
abline(lm(eruptions~waiting,data=faithful),col="red")

# Exercise2.1
data("airquality")
head(airquality)

mod = lm(Temp~Ozone, data=airquality)
plot(Temp~Ozone,data=airquality)
abline(lm(Temp~Ozone,data=airquality),col="red")

# Exercise2.2
mod1 = lm(eruptions~waiting,data=faithful)
summary(mod1)
# From the summary table, since the estimate value for waiting, which is the slope, is 0.075628. 
# That means if the waiting time increase by 1 unit, the duration of the eruption will increase by 0.075628. (positively correlated)

# Exercise 3
res = residuals(mod)
head(res)
RSS = sum(res^2)

str(summary(mod))
# from the summary list, the df is 2 114 2
RSE = sqrt(RSS/summary(mod)$df[2])
# RSE for the model with ozone as independent variable and temp as dependent variable is 6.8189

MSE = RSS/summary(mod)$df[2]
# MSE for the model with ozone as independent variable and temp as dependent variable is 46.498


