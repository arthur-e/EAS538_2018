setwd("D:/UM/SNRE/Winter 2017/NRE 538 Statistics/Week 8 Lab")
data("airquality")

#Exercise 1 Try to plot and calculate the correlation coefficients (r) between the waiting time between eruptions and the duration of the eruption for the Old Faithful geyser in Yellowstone National Park
data("faithful")
head(faithful)
plot(faithful$waiting,faithful$eruptions)
abline(lm(eruptions~waiting,data=faithful),col="red")
cor(faithful$eruptions,faithful$waiting)
#The correlation coefficient between waiting time and duration of eruption is 0.901

#Exercise 2
#Build another linear model with another independent variable, Ozone, and plot it with estimated regression line.
plot(airquality$Ozone,airquality$Temp)
abline(lm(Temp~Ozone,data=airquality),col="red")
modair=lm(formula=Temp~Ozone,data=airquality)
summary(modair)
#The intercept is significantly different from 0
#The slope of estimated line is significantly greater 0.Therefore, temperature is significantly and positively related to ozone concentration. 
#With each unit (ppb, part per billion) of increase in ozone concentration, the temperature increase by 0.2 degrees F


#Build a linear model to explain if you see longer eruption time if you wait for longer in the faithful dataset.
modspring=lm(formula=eruptions~waiting,data=faithful)
summary(modspring)
#The slope of estimated line is significantly greater than 0.
#The duration of eruption is significantly and positively related to waiting time.
#We can expect to see longer eruption time if we wait longer.
#With each minute increase in waiting time, the eruption duration increase by 0.07 minute (about 4.2 sec)

#Exercise 3 Calculat the residual standard error (RSE) and mean square error (MSE) manually for the model you built last time (Ozone as the independent variable).
res=residuals(modair)
RSS=sum(res^2)
RSE=sqrt(RSS/114) #The degree of freedom is in the summary table (Residual standard error: 6.819 on 114 degrees of freedom)
#Or RSE=sqrt(RSS/summary(modair)$df[2])
RSE
#RSE=6.819

MSE=RSS/114
#Or RSE=sqrt(RSS/summary(modair)$df[2])
MSE
#MSE=46.49759

#Confirmation
anova(modair)
