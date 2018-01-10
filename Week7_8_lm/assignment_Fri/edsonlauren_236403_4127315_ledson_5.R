##Lauren Edson
##Lab 6 exercises 1, 2, and 3


####
###Exercise 1
##Try to plot and calculate the correlation coefficients between the 
##waiting time between eruptions and the duration of the eruption for 
##the Old Faithful geyser in Yellowstone National Park. The dataset is built 
##in R named faithful.

data(faithful)
head(faithful)

plot(faithful$eruptions~faithful$waiting)
cor(faithful$waiting, faithful$eruptions)
#correlation coefficient is 0.9008112
#The plot seems to show a linear relationship that has clusters
#at the low end of waiting time and at the high end of waiting time.
#the data is sparse in the middle area of waiting times.



####
###Exercise 2
##Build another linear model with another independent variable, Ozone, and 
##plot it with estimated regression line.

data("airquality")
head(airquality,15)

model = lm(Temp~Ozone, data=airquality)
summary(model)
##R^2 is 0.4877
##Intercept: 69.41072 (st err 1.02971)
##Slope: 0.20081 (st err  0.01928)
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")
#There is a positive, linear relationship between ozone and temperature. This means 
#that as Ozone increases by one unit, the temp increases by 0.2 units. 


##Build a linear model to explain if you see longer eruption time if you wait
##for longer in the faithful dataset.

faithfulmodel = lm(eruptions~waiting, data=faithful)
summary(faithfulmodel)
##R^2 is 0.8115
##Intercept: -1.874016 (st err 0.160143)
##Slope:  0.075628 (st err 0.002219)
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")
#There is a positive, linear relationship. The interceppt is at -1.87 
#and the slope is 0.0756. This means that as waiting time increases by 1 minute, 
#the eruption duration will increase by 0.075 minutes (or approx. 4.5 seconds)



####
###Exercise 3
##Calculat the residual standard error (RSE) and mean square error (MSE) 
##manually for the model you built last time (Ozone as the independent variable).

res1 = residuals(model)
RSS1 = sum(res1^2) 
RSS1 #RSS1 = 6.818913
RSE1 = sqrt(RSS1/summary(model)$df[2])
RSE1 #RSE1 = 6.818

RMSE1 = RSS1/114
RMSE1 #RMSE1 = 46.49759


