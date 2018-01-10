### Exercise 1 ###

data("faithful")
head(faithful, 15)

plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")

cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")
# The correlation coefficient between waiting time and eruption duration is 0.90
# Waiting time and eruption duration are strongly, positively correlated

### Exercise 2 ###
## Part 1 ##

data("airquality")
head(airquality, 15)

mod.air = lm(Ozone~Wind, data=airquality)
summary(mod.air)
# Wind and ozone are negatively correlated
# For each unit increase in wind, there is a 5.5509 unit decrease in ozone
# R-squared = 0.3619 (36.19% of variability in ozone can be explained by wind)
# p-value = 9.27e-13
plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")

## Part 2 ##

mod.faith = lm(eruptions~waiting, data=faithful)
summary(mod.faith)
# For each unit increase in waiting time to see an eruption, the eruption lasts 0.0756 units longer
# R-squared = 0.8115 (81.15% of variability in eruption duration can be explained by waiting time)
# p-value < 2e-16

### Exercise 3 ###

res = residuals(mod.air)
RSS = sum(res^2)
RSS
# Residual Sum of Squares = 79859.01
RSE = sqrt(RSS/summary(mod.air)$df[2])
RSE
# Residual Standard Error = 26.46729

MSE = RSS/summary(mod.air)$df[2]
MSE
# Mean Square Error = 700.5177

# Compare to ANOVA to confirm 
anova(mod.air)
# ANOVA resuts indicate that MSE = 701


