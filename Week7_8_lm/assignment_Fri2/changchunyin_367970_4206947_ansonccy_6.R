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

aa = airquality[which(is.na(airquality$Ozone)==FALSE),]
mod = lm(Temp~Ozone, data=aa)
plot(Temp~Ozone,data=aa)
abline(lm(Temp~Ozone,data=aa),col="red")

# Exercise2.2
mod1 = lm(eruptions~waiting,data=faithful)
summary(mod1)
# From the summary table, since the estimate value for waiting, which is the slope, is 0.075628. 
# That means if the waiting time increase by 1 unit, the duration of the eruption will increase by 0.075628. (positively correlated)

# Lab8 
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

# Exercise 4 

summary(mod)
# adjusted R2 = 0.4832 

SSY=deviance(lm(aa$Temp~1))
RSS_adjusted=RSS/(114)
SSY_adjusted=SSY/(115)

R2= 1-(RSS_adjusted/SSY_adjusted)
R2
# The manually calculated adjusted R2 is 0.4832, which is the same as the adjusted R2 from summary table. 

# Exercise 5

library(lmtest)
dwtest(mod, alternative=c("two.sided"))
# As the p-value is significant, there is true autocorrelaion which means the residual is not independent

plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red")
# The residuals distance from the center zero lines varies, so the residual is heteroskedastic.

bptest(mod)
# The p-value is not significant which means we fail to reject the null hypothesis that the residual is homoscedastic.
# So the residual is heteroskedastic. 

qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
# The result follows a normal distribution 

