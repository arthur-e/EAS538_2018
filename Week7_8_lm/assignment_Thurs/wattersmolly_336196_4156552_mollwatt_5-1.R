data("airquality")
head(airquality, 15)

###EXERCISE 1
#compare waiting time between Old Faithful eruptions with duration of eruptions

data("faithful")
plot(faithful$eruption,faithful$waiting)
cor(faithful$eruptions,faithful$waiting)
#cor=0.09008112, so there is a positive linear correlation between waiting time and eruption time.

###EXERCISE 2
AirQualMod = lm(Temp~Ozone, data=airquality)
summary(AirQualMod)
#Intercept=69.41072
#Both p-values are less than 0.05 (<2e-16), so the intercept and the slope is significantly different from 0
#Ozone coefficient=0.20081; Ozone increases by 0.20081 with every unit increase in temperature

OldFaithMod = lm(eruptions~waiting, data=faithful)
summary(OldFaithMod)
#Intercept=-1.874016 
#Both p-values are less than 0.05 (<2e-16), so the intercept and the slope are significantly different from 0
#Waiting time coefficient=0.075628; Eruption time increases by 0.075628 with each unit increase in waiting time.

###EXERCISE 3
AirQualRes = residuals(AirQualMod)
AirQualRSS = sum(AirQualRes^2)
AirQualRSE = sqrt(AirQualRSS/summary(AirQualMod)$df[2])
AirQualMSE = AirQualRSS/summary(AirQualMod)$df[2]
#AirQualRSE = 6.818914
#AirQualMSE = 46.49759
