#Erin Barton 
#27 Feb 2017
#NRE538-Lab08


data("airquality")

#Exercise 4: Calculate the adjusted R^2 manually for Temp~Ozone
ex4 <- lm(Temp~Ozone, data=airquality)
summary(ex4)

mod_ex4 <- lm(Temp~Ozone, data=airquality)
res_ex4 <- residuals(mod_ex4)
RSS_ex4 <- sum(res_ex4^2)

k=2
n=length(res_ex4)


data_noNA <- subset(airquality, Ozone!="NA" & Temp!="NA")
SSY_ex4 <- deviance(lm(Temp~1, data=data_noNA))
#114 is the df of the residuals <- lose two x's because they are being used to describe the model
SSY_ex4alt <- var(airquality$Temp)*115

R2=1-((RSS_ex4/(n-k))/(SSY_ex4/(n-1)))
R2


#Exercise 5: check residual independency, homoscedasticity, 
#and normality for the model with ozone as the IV and interprest the results briefly

lm(Temp~Ozone, data=airquality)
#Coefficients:
#(Intercept)        Ozone  
#  69.4107       0.2008  

ozone_mod <- lm(Temp~Ozone, data=airquality)
summary(ozone_mod)
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality),col="red")
res.oz <- residuals(ozone_mod)
RSS.oz <- sum(res.oz^2)
RSS.oz
#[1] 5300.725

RSE.oz <- sqrt(RSS.oz/ozone_mod$df)
RSE.oz
#[1] 6.818914

MSE.oz=RSS.oz/ozone_mod$df
MSE.oz
#[1] 46.49759

#Check Residual Independency
plot(residuals(ozone_mod))
dwtest(ozone_mod, alternative = c("two.sided"))
#Durbin-Watson test
#data:  ozone_mod
#DW = 1.1833, p-value = 7.276e-06
#alternative hypothesis: true autocorrelation is not 0

#The Durbin-Watson test confirms what plotting the residuals seems to show: their is a spatial pattern (autocorrelation) in the data.In other words, the data is not indpendent.  
#This is what a low p-value on the Durbin-Watson test indicates, and the test shows ozone_mod to have a very low p-value. 

#Check Homoscadicity
plot(ozone_mod, which=c(1))
#This plot helps to see whether the variance of residuals is constant. As the points are not evenly distributed 
#across the plot, the graph suggest the variance of the residuals is not constant. 
bptest(ozone_mod)
#studentized Breusch-Pagan test
#data:  ozone_mod
#BP = 3.2346, df = 1, p-value = 0.0721

#This test checks to see if the variance is constant. The lower the p-value, the more constant the variance. 
#These results show a p-value higher than 0.05, suggesting that the variance of the residuals is not as constant 
#as we would like (p-value≤0.05)

#Check Normality
qqnorm(residuals(ozone_mod))
qqline(residuals(ozone_mod),col="red")

shapiro.test(residuals(ozone_mod))
#Shapiro-Wilk normality test
#data:  residuals(ozone_mod)
#W = 0.94867, p-value = 0.000226

#These results suggest the data is not quite normal, as the data deviates from the qqplot line 
#and the shapiro-wilkes test has a low p-value. 

