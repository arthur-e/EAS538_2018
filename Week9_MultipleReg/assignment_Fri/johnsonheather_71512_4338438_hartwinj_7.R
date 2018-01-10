# Exercise 1

setwd("C:/Users/Owner/Desktop/U of M Masters Program/Winter 2017/NRE 538/Lab/Assignments/Assignment 9")
> summary(airquality)
Y=seq(from=0, to=20, by=0.1)
X1=(Y-2)/3+runif(length(Y), min=0.5, max=1)
X2=(Y+3)/2+runif(length(Y), min=0.5, max=1)
df=as.data.frame(cbind(Y,X1,X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))
cor(X1,X2)
#[1] 0.9957884
#By decreasing the noise to "1", in the original X1 and X2 formulas, there was a higher correlation (seen as the correlation coefficient gets closer to "1");
#less randomization of numbers were used in the string since less of the dataset was used.
Y=seq(from=0, to=20, by=0.1)
X1=(Y-2)/3+runif(length(Y), min=0.5, max=10)
X2=(Y+3)/2+runif(length(Y), min=0.5, max=10)
df=as.data.frame(cbind(Y,X1,X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))
cor(X1,X2)
#[1] 0.3739453
#By increasing the noise to "10", in the original X1 and X2 formulas, there is less correlation between X1 and X2 (seen as the correlation coefficient gets closer to "0");
#there's more randomization of numbers; more dataset was used in the string.

#Exercise 2
#Model 1 RSS = 109(8.306²) = 7519.87
#Model 2 RSS = 108(8.039²) = 6979.56
#F-stat = (7519.87 - 6979.56/2-1)/(6979.56/108) = 8.36
#p-value

#1
mod3=lm(Temp~Wind+Solar.R+Ozone, data=airquality1)
summary(mod3)
anova(mod1,mod2,mod3)
#Adding Ozone as a third independent variable to the model does explain more variance;
#the F stat increased dramatically to 35.65 with a p value 4.729e-16. 
#The probability for mod1 and mod2 to explain the same amount of variance is significantly
#lower than the other p values. Which would intuitively make sense since mod3 is nested and can be
#transformed to mod2 or mod 1.  

#Exercise 3
AIC(mod1)
[1] 788.9671
AIC(mod2)
[1] 782.6779
AIC(mod3)
[1] 747.5795
#According to the AIC output, the best fit model to explain effect on airquality,
#is the model using the three independent variables Wind, Solar and Ozone.  