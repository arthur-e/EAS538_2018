## set working directory
setwd("/docs/umich/coursework/538/labs/lab08")

## install dplyr
library(dplyr)

## import data
data("airquality")

## subset data to remove empty values
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)


## begin exercise 1
## generate y, x1, x2, and combine into a single data frame
y = seq(from=0, to=20, by=0.1)
x1 = log((y-2)/3 + runif(length(y), min=0.2, max=20))
x2 = log((y+3)/2 + runif(length(y), min=0.2, max=20))
df = as.data.frame(cbind(y, x1, x2))

## generate linear model for the data frame variables
summary(lm(y~x1+x2, data=df))
## after inreasing the noisiness of the data and performing a log transformation,
## the modeled regression coefficients more closly match their actual values


## begin exercise 2
## generate models
mod1 = lm(Temp~Wind, data=airquality)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)

## calculate residuals and RSS for each model
res1 = residuals(mod1)
RSS1 = sum(res1^2)
res2 = residuals(mod2)
RSS2 = sum(res2^2)

## calculate F statistic
modF = ((RSS1-RSS2)/(3-2))/(RSS2/(111-3))

## calculate p value
## hrm...

## generate third linear model
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
anova(mod2, mod3)
## mod3 explains significantly greater variation than mod2, with a probability
## of almost zero that mod2 explains as much


## begin exercise 3
AIC(mod1, mod2, mod3)
## the AIC results suggest that the difference between model 1 and model 2 is not
## significant, with a difference <10, but the difference between model 3 and the
## other two models is significant, with differences >30