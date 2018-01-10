#Exercise 1

data("faithful")
head(faithful, 15)

#Calculate coeffieicnets between the waiting time between eruptions 
#and duration of eruptions.

cor(faithful[, c("eruptions","waiting")], use="na.or.complete")

#          eruptions   waiting
#eruptions 1.0000000 0.9008112
#waiting   0.9008112 1.0000000

#Plot

plot(faithful[, "eruptions"]~faithful[, "waiting"])

#comments
#Plot and Cor show a positive correlation

#Exercise 2
#build another linear model with another indp. variable, Ozone 
# plot with a regression

mod1 = lm(Temp~Ozone, data=airquality)
summary(mod)

plot(Temp~Ozone, data=airquality)  
abline(lm(Temp~Ozone, data=airquality), col="red")


#The two variables are significantly different from 0 (p value=2.2e-16)
#For each 1 unit of Ozone that increases than temperature increases at an estimated 0.2 degrees (p=-2e-16)
#The intercept starts as 69.41072, at 0 ozone our model predicts a temperature of 69.41 degrees

#2.1
#Build a linear model to explain if you see eruption time if you
#wait longer in the faithful dataset

mod2 = lm(eruptions~waiting, data=faithful)
summary(mod2)

plot(eruptions~waiting, data=faithful)  
abline(lm(eruptions~waiting, data=faithful), col="red")

#The two variables are significantly different from 0 (p=2.2e-16)
#For each 1 unit (min) of waiting, the eruption time increases 0.07 (mins) (p=2.2e-16)

#you would expect a longer eruption time, the longer that you wait.

#Exercise 3
#Calculate the residual standard error (RSE) and mean square error (MSE) 
#manually for the model you built last time (Ozone as the independent variable).

Res = residuals(mod1)
RSS = sum(Res^2)
RSE = sqrt(RSS/summary(mod1)$df[2])
RSE
#RSE = 6.818914

MSE = RSS/summary(mod1)$df[2]
MSE
#MSE=46.49759



