data("airquality")
head(airquality, 20)

library(dplyr)
#^Need this to complete the next step

#Next, remove N/A's to clean up the dataset
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

#Plot
pairs(airquality[,c(1:4)])

#Calculate correlations 
cor(airquality[,c(1:4)], use="na.or.complete")
#^Notice the high correlation between wind/ozone and temp/ozone


####Exercise 1: Manipulate the above code to reduce the 
####correlation between X1 and X2 and recalculate the regression 
####coefficients. Explain what you find. Hint: Increase the random 
####noise of X1 and X2 so that the two can be less correlated.

Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=20)
X2 = (Y^3)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1+X2, data=df))
plot(X1~X2)
cor(X1,X2)
# cor = 0.2166263

#By increasing the max and therefore the noise for X1, the correlation decreases significantly.


###Comparing models

mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)
anova(mod1, mod2)

####Exercise 2 [Bonus, 2 pts] Challenge yourself by calculateing the 
#RSS of the two models, F statistics (1 pt) and the p-value (1 pt) all 
#by yourself (i.e. not using the anova() function)

res1 = residuals(mod1)
RSS1 = sum(res1^2)
RSS1

res2 = residuals(mod2)
RSS2 = sum(res2^2)
RSS2

#Not sure how to manually calculate the F statistic and p-value...ran out of time!

#Compare a third model (mod3) with ozone varaible as the third independent 
#variable to mod1 and mod2 above. Does it explain more variance? Does it 
#give you reasonable estimate of regression coefficients?

mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)

anova(mod1, mod2, mod3)

####Exercise 3
#Compare the AIC of the three models above mod1, mod2, and mod3. Do you 
#have different conclusion in terms of which model performs better?

#Lower AIC is better, therefore model 3 performs best. Results below:

AIC(mod1, k = 2)
#788.9671
AIC(mod2, k = 2)
#782.6779
AIC(mod3, k = 2)
#747.5795