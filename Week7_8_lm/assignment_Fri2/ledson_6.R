##Lauren Edson
##Lab 6 exercises 4 and 5


data("airquality")
head(airquality,15)

######################################################
###Exercise 4
##Calculate the adjust R2 manually.

airquality.cleaned=airquality[which(is.na(airquality$Ozone)==FALSE),]
cleaned=subset(airquality, is.na(Ozone)==FALSE)


model.oz.clean = lm(Temp~Ozone, data=cleaned)
summary(model.oz.clean)
plot(Temp~Ozone, data=cleaned)
abline(lm(Temp~Ozone, data=cleaned), col="red")
#Multiple R-squared:  0.4877,	Adjusted R-squared:  0.4832 
#114 degrees of freedom


res = residuals(model.oz.clean)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(model.oz.clean)$df[2])
SSY = deviance(lm(cleaned$Temp~1))
SSY #13617.88

SSY1 = var(cleaned$Temp)*152 

#adjusted
R2 = 1 - (RSS/(115-2))/(SSY/(115-1))
R2 #0.4831736

##this manually calculated R squared does match the 
## summary of the model.



######################################################
###Exercise 5
##Check residual independency, homoscedasticity, and normality for the model 
##with ozone as the independent variable and interpret the results briefly.



##Test for autocorrelation, i.e. independency########
install.packages('lmtest')
library(lmtest)
dwtest(model.oz.clean , alternative=c("two.sided"))
# Durbin-Watson test
# data:  model.oz.clean
# DW = 1.1833, p-value = 7.276e-06
# alternative hypothesis: true autocorrelation is not 0
##So this tells us that there is an autocorrelation. The amount of ozone
##is related to the ozone from the day before


###Check homoscedasticity #################
plot(residuals(model.oz.clean)~fitted(model.oz.clean))
abline(lm(residuals(model.oz.clean)~fitted(model.oz.clean)), col="red")
#plot(model.oz.clean, which=c(1))

##the plot of residuals shows that the variances are not consistent
##This means that they are not homoscedastic

bptest(model.oz.clean)
#studentized Breusch-Pagan test
#data:  model.oz.clean
#BP = 3.2346, df = 1, p-value = 0.0721

## the bp test shows a p-value that is not significant, meaning that the
##residuals of this model are homoscedastic


###Check Normality#######################
qqnorm(residuals(model.oz.clean))
qqline(residuals(model.oz.clean), col="red")
#plot(model.oz.clean, which=c(2))

shapiro.test(residuals(model.oz.clean))
# Shapiro-Wilk normality test
# data:  residuals(model.oz.clean)
# W = 0.94867, p-value = 0.000226

##These tests show us that the assumption
##of normality is a safe assumption


##plot the model to check plots
plot(model.oz.clean)



