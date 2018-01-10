# Import NY air quality data
data("airquality")

#from assignment 5:
mod1 = lm(Temp~Ozone, data=airquality)
summary(mod1)

## EXERCISE 4 

airquality2 = airquality[which(is.na(airquality$Ozone)==FALSE),]
mod2 = lm(Temp~Ozone, data=airquality2)
res2 = residuals(mod2)
RSS2 = sum(res2^2)

num = RSS2/(length(airquality2$Ozone)-2)
den = var(airquality2$Temp)

AR2 = 1 - (num/den)
AR2 #0.4832134

#check
summary(mod1) #Adjusted R-squared = 0.4832
summary(mod2) #Adjusted R-squared = 0.4832

#After removing the NAs, the manually calculated Adjusted R-squared (0.4832) equals the Adjusted R-squared from the summary function of the original model.

## EXERCISE 5

install.packages('lmtest')
library(lmtest)

#Residual Independency
plot(residuals(mod1))
#From the plot, it looks like there is a pattern, so it seems like the residuals are autocorrelated 

dwtest(mod1, alternative=c("two.sided"))
#DW test has a p-value of 7.276e-06, which means that the residuals are autocorrelated and NOT indepedent

#Residual Homoskedasticity
plot(residuals(mod1)~fitted(mod1))
abline(lm(residuals(mod1)~fitted(mod1)), col="red") #it looks like the residuals are not homeskedastic because the residuals change with an increase in fitted value 

bptest(mod1)
# The bptest has a p-value of 0.0721, so the residuals ARE homoskedastic at the .05 level (we cannot reject the null hypothesis of homoskedasticity)

#Residual Normality

qqnorm(residuals(mod1))
qqline(residuals(mod1), col="red") #From the QQ Plot, it looks like it there is not a normal distribution because the residuals deviate from the line

shapiro.test(residuals(mod1)) #The p-value is 0.000226, so the residuals are not normally distributed. 


