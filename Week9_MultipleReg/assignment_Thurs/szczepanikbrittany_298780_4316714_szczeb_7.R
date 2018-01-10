## Exercise 1
#prior to altering the noise, the correlation is 0.9638
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=10)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=10)
df = as.data.frame(cbind(Y, X1, X2))
#after increasing the max value in the noise for X1 and X2 to 10, the correlation is now 0.3679, which is below 0.50 (which is what we want)

################################################################

##Exercise 2
#Bonus
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)

res1 = residuals(mod1)
RSS1 = sum(res1^2)
RSS1

RSE1 = sqrt(RSS1/summary(mod1)$df[2])
RSE1
#matches data output

RMSE1 = RSS1/153
RMSE1

MSE1 = RSS1/summary(mod1)$df[2]
MSE1

anova(mod1)
#MSE1 matches the anova output

SSY1 = deviance(lm(airquality$Temp~1))
SSY1

F1 = ((SSY1-RSS1)/1)/MSE1
F1

mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)

res2 = residuals(mod2)
RSS2 = sum(res2^2)
RSS2

RSE2 = sqrt(RSS2/summary(mod2)$df[2])
RSE2
#matches data output

RMSE2 = RSS2/153
RMSE2

MSE2 = RSS2/summary(mod2)$df[2]
MSE2

anova(mod2)
#MSE2 matches the anova output

SSY2 = deviance(lm(airquality$Temp~1))
SSY2

F2 = ((SSY2-RSS2)/2)/MSE2
F2


#Calculate a third model
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)

mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)

anova(mod1, mod2, mod3)

#The R^2 value for model 3 is 0.4999, which is higher than models 1 and 2, which were 0.2472 and 0.3014, respectively.
    #Due to the larger R^2 value in mod3, we can reasonably conclude that it explains more of the observed variance when compared to the other two models
#In terms of the regression coefficients, ozone is significantly different than temperature, but the p-values for both wind and solar are above 0.05, which suggests that they is a relationship between those two variables and temperature and may not be entirely different
    #Additionally, the standard error, when compared to the coefficients themselves, is pretty high for wind and solar, suggesting low confidence in the estimate.  The stardard error for solar, for example, is higher than the actual estimate.


######################################################################

##Exercise 3
AIC(mod1)
AIC(mod2)
AIC(mod3)

#based on the three AIC scores for the model, we would want to choose mod3 because it has the lower AIC score
#as discussed in lecture and lab, small differences (of about 10 or less) in AIC scores don't matter as much, but mod3 is 35 less than mod2, which from my understanding, is significant


