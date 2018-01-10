#####Exercise 1
##Try to plot and calculate the correlation coefficients between the waiting time between eruptions and the duration of the eruption for the Old Faithful geyser in Yellowstone National Park. The dataset is built in R named faithful.

data(faithful)
head(faithful)

plot(eruptions~waiting, data = faithful)

cor(faithful$eruptions,faithful$waiting)
# R = .9008112
#The high correlation coefficient suggests that the relationship between the variables can be described by a linear model.
#The plot reveals the same thing visually.

#####Exercise 2
##Build another linear model with another independent variable, Ozone, and plot it with estimated regression line.
ozoneMOD = lm(Temp~Ozone, data=airquality)
summary(ozoneMOD)

plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")
#intercept = 69.41072 with a p-value of <2e-16 *** showing us that it is significantly different from 0.
#slope = 0.20081, meaning that when you increase 1 unit of ozone, temperature increases 0.20081 units

##Build a linear model to explain if you see longer eruption time if you wait for longer in the faithful dataset.
##waiting = independent variable, eruptions = response variable

geyserMOD = lm(eruptions~waiting, data = faithful)
summary(geyserMOD)

plot(eruptions~waiting, data = faithful)
abline(lm(eruptions~waiting, data = faithful), col="red")
#intercept = -1.874016, with a p-value of <2e-16 *** showing that it is significantly different from 0.
#slope is .075628, meaning that when you increase 1 unit of wind, temperature increases .075628 units

#####Exercise 3
##Calculate the residual standard error (RSE) and mean square error (MSE) manually for the model you built last time (Ozone as the independent variable).

#manually calculate the residual standard error (RSE)
resids = residuals(ozoneMOD) ##calculate the residuals
RSS = sum(resids^2) ##calculate the residual sum of squares
RSE = sqrt(RSS/summary(ozoneMOD)$df[2])
RSE # = 6.818914
##The RSE is the variance of all residuals. It expresses the variability of the dependent variable that is not explained by our model.

#manually calculate the mean square error (MSE)
MSE = RSS/summary(ozoneMOD)$df[2]
MSE #MSE = 46.49759
##The MSE is the standard deviation of all residuals.

#check the MSE with the anova() function
anova(ozoneMOD)
#Analysis of Variance Table

#Response: Temp
#            Df  Sum Sq   Mean Sq   F value    Pr(>F)    
#Ozone       1   5046.3   5046.3    108.53     < 2.2e-16 ***
#Residuals 114   5300.7    46.5 

#The MSE (Mean Sq) is roughly the same in the ANOVA table. 




