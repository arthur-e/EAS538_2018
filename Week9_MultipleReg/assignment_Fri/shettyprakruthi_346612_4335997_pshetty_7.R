data("airquality")
head(airquality, 15)
library(dplyr)

airquality1 = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

pairs(airquality1[,c(1:4)])

#Exercise 1
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.3, max=20)
X2 = (Y+3)/2 + runif(length(Y), min=0.7, max=15)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
#M1: Regression coefficient X1: 0.32584 
summary(lm(Y~X2, data=df))
#M2: Regression coefficient X2: 0.66941
summary(lm(Y~X1+X2, data=df))
#M3
#Regression coefficient X1: 0.22261
#Regression coefficient X2:0.60755
#Comment: The Regression coefficient for M3 for variable X1 and X2 is similar to the regression coefficient calculated for M1 and M2 for variables X1 and X2 respectively
#When the correlation between the explanatory varibbles is less, the effect on regression coffiecient calculated using all those explanotory variable is not biased. This bias was clearly seen while examining the regression coefficients for variables before making changes to it in this exercise.
plot(X1,X2)
cor(X1, X2)
#r:0.2172943

mod1 = lm(Temp~Wind, data=airquality1)
summary(mod1)

mod2 = lm(Temp~Wind+Solar.R, data=airquality1)
summary(mod2)

mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality1)
summary(mod3)

#BONUS
#RSS mod1
res1 = residuals(mod1)
RSS1 = sum(res1^2)
#RSS1 = 7520.672

#RSS mod2
res2 = residuals(mod2)
RSS2 = sum(res2^2)
#RSS1 = 6979.506

#F-statistic
Fmod = ((RSS1-RSS2)/(mod1$df-mod2$df))/(RSS2/(mod2$df))
anova(mod1,mod2)
#Fmod: 8.373933

1-pf(Fmod,1,109)
#p-value:0.004597954


#Exercise 2
anova(mod1,mod2,mod3)
#The model 3 exhibits more variance compared to model1 and model 2 as explained by its regression coefficient which is 4996.6 (the other 2 being 7520.7 and 6979.5)
#The F-statistic is 42.463 and p value being extremely low which indicates that the probablity with which model 1 can explain same amount of varience in model 3 is nearly 0.

#Exercise 3
AIC(mod1)
#788.9671
AIC(mod2)
#782.6779
AIC(mod3)
#747.5795 !WINNER!
#Comment: Model3 has the smallest AIC value thus a better performing model compared to 1 and 2. Also since the difference between their AIC value is greater than 10 this comparision is significant.

