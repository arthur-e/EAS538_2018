##EXERCISE 1
##Try to plot and calculate the correlation coefficients between the waiting time between eruptions and the duration of the 
##eruption for the Old Faithful geyser in Yellowstone National Park. The dataset is built in R named faithful.
data("faithful")
cor(faithful$eruptions,faithful$waiting)
#correlation coefficient: 0.9008

plot(faithful$eruptions, faithful$waiting)



##EXERCISE 2
## 1. Build another linear model with another independent variable, Ozone, and plot it with estimated regression line.
data("airquality")
head(airquality)

mod2=lm(Temp~Ozone, data = airquality)

plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")

## 2. Build a linear model to explain if you see longer eruption time if you wait for longer in the faithful dataset.

faithful.lm=lm(waiting~eruptions, data=faithful)
summary(faithful.lm)

plot(faithful$eruptions, faithful$waiting)
abline(lm(waiting~eruptions, data=faithful), col="red")

##Longer wait times are significantly correlated with longer eruption times. Intercept: 33.4744, slope: 10.7296. R-sq: 0.8115 


##EXERCISE 3
## Calculate the residual standard error (RSE) and mean square error (MSE) manually for the model you built 
## last time (Ozone as the independent variable).

res = residuals(faithful.lm)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(faithful.lm)$df[2])

RMSE = RSS/153
#RMSE: 61.72

MSE = RSS/summary(faithful.lm)$df[2]
#MSE: 34.97

anova(faithful.lm) #MSE: 35







