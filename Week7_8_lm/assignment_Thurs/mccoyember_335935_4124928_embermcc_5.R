# Import Yosemite Old Faithful geyser Data
data("faithful")
head(faithful)

# Import NY air quality data
data("airquality")
head(airquality, 15)

# EXERCISE 1

cor(faithful$eruptions, faithful$waiting)
plot(faithful$eruptions,faithful$waiting)

#There is a correlation coefficent of .9008112. Based on the plot, it looks like there is a positive linear relationship between eruption duration and waiting time.  

# EXERCISE 2.1

mod1 = lm(Temp~Ozone, data=airquality)
summary(mod1)
#Intercept = 69.41072, slope = .20081, both have p value =  <2e-16 so the intercept/slope values are significantly different from 0.
#This means that as ozone increases by one unit, the temperature increases by .20081 units. 

plot(Temp~Ozone, data=airquality)
abline(mod1, col="red")

# EXERCISE 2.2 

mod2 = lm(eruptions~waiting, data=faithful)
summary(mod2)
#Intercept = -1.874016, slope =  0.075628, both have p value =  <2e-16 so the intercept/slope values are significantly different from 0.
## This means that as waiting time increases by one unit, eruption duration increases by 0.075628 units.

plot(eruptions~waiting, data=faithful)
abline(mod2, col="red")

# EXERCISE 3 

res = residuals(mod1)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod1)$df[2])
RSE # 6.818914

summary(mod1) #114 degrees of freedom

RMSE = RSS/116

MSE = RSS/summary(mod1)$df[2]
MSE #46.49759

#check if correct 
anova(mod1) 
#mean squared = 46.5 so correct because matches MSE!


