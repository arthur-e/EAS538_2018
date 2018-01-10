## set working directory
setwd("/docs/umich/coursework/538/labs/lab06")


## begin exercise 1
## import data
data("faithful")

## plot data
plot(eruptions~waiting, data=faithful)
## the data appear to have a positive correlation, with eruption durations increasing
## as the time between eruptions increases

## calculate correlation coefficients
cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")
## the test shows a strong positive correlation between the variables


## begin exercise 2
## import data
data("airquality")

## generate linear model for temperature and ozone
mod_air = lm(Temp~Ozone, data=airquality)
summary(mod_air)
## the positive correlation between the variables is significant

## plot data used for linear model
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")
## the plot demonstrates the results of the modeling with the best fit line

## generate and summarize linear model for eruption duration and wait time
mod_faith = lm(eruptions~waiting, data=faithful)
summary(mod_faith)
## the summary table shows that wait time has a strong and significant effect on
## eruption durations

## begin exercise 3
## calculate residuals
res = residuals(mod_air)
RSS = sum(res^2)
MSE = RSS/summary(mod_air)$df[2]
print(paste0("MSE = ", MSE))
RSE = sqrt(RSS/summary(mod_air)$df[2])
print(paste0("RSE = ", RSE))
## the RSE matches the value obtained from the linear model (6.819),
## which is approximately 10% of the intercept value (69.41072)