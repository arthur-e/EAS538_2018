data("airquality")
head(airquality,15)
library(dplyr)
airquality1=airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
head(airquality1)
pairs(airquality1[,c(1:4)])
cor(airquality[,c(1:4)], use="na.or.complete")

# Exercise 1
Y=seq(from=0,to=20,by=0.1)
X1=(Y-2)/3+runif(length(Y),min=0.5,max=2)
X2=(Y+3)/2+runif(length(Y),min=0.5,max=2)
df=as.data.frame(cbind(Y,X1,X2))
summary(lm(Y~X1,data=df))    # Estimate X1=2.83209
summary(lm(Y~X2,data=df))    # Estimate X2=1.95685
summary(lm(Y~X1+X2,data=df)) # Estimate X1=0.82615; Estimate X2=1.41249
plot(X1~X2)
cor(X1,X2) # cor=0.9659445

Y=seq(from=0,to=20,by=0.1)
X1=(Y-2)/3+runif(length(Y),min=0.5,max=5)
X2=(Y+3)/2+runif(length(Y),min=0.5,max=5)
df=as.data.frame(cbind(Y,X1,X2))
summary(lm(Y~X1,data=df))    # Estimate X1=2.03994
summary(lm(Y~X2,data=df))    # Estimate X2=1.77750
summary(lm(Y~X1+X2,data=df)) # Estimate X1=0.87655; Estimate X2=1.26782
plot(X1~X2)
cor(X1,X2) # cor=0.7530003

Y=seq(from=0,to=20,by=0.1)
X1=(Y-2)/3+runif(length(Y),min=0.5,max=8)
X2=(Y+3)/2+runif(length(Y),min=0.5,max=8)
df=as.data.frame(cbind(Y,X1,X2))
summary(lm(Y~X1,data=df))    # Estimate X1=1.2953
summary(lm(Y~X2,data=df))    # Estimate X2=1.29108
summary(lm(Y~X1+X2,data=df)) # Estimate X1=0.71353; Estimate X2=1.02872
plot(X1~X2)
cor(X1,X2) # cor=0.5469083

Y=seq(from=0,to=20,by=0.1)
X1=(Y-2)/3+runif(length(Y),min=0.5,max=11)
X2=(Y+3)/2+runif(length(Y),min=0.5,max=11)
df=as.data.frame(cbind(Y,X1,X2))
summary(lm(Y~X1,data=df))    # Estimate X1=0.86582
summary(lm(Y~X2,data=df))    # Estimate X2=0.93054
summary(lm(Y~X1+X2,data=df)) # Estimate X1=0.61824; Estimate X2=0.74001
plot(X1~X2)
cor(X1,X2) # cor=0.3247468
## As the noise of X1 and X2 increases (less correlated between X1 and X2),
## the difference of coefficient estimate between model with single predictor variable and model with two predictor variables decreases.
## Which shows that multi-collinearity (predictor variables are highly correlated with each other)
## will make it difficult to interpret the effect of each idividual predictor variable on response variables when other predictors are held constant

# Exercise 2
mod1=lm(Temp~Wind,data=airquality1)
summary(mod1)
mod2=lm(Temp~Wind+Solar.R,data=airquality1)
summary(mod2)
mod3=lm(Temp~Wind+Solar.R+Ozone,data=airquality1)
summary(mod3)

res1=residuals(mod1)
RSS1=sum(res1^2) # RSS1=7520.672
n=111
p1=2
res2=residuals(mod2)
RSS2=sum(res2^2) # RSS2=6979.596
n=111
p2=3
res3=residuals(mod3)
RSS3=sum(res3^2) # RSS3=4996.601
n=111
p3=4
## F statistics of mod1 and mod2
F12=(((RSS1-RSS2)/(p2-p1))/(RSS2/(n-p2))) # F12=8.373933
F23=(((RSS2-RSS3)/(p3-p2))/(RSS3/(n-p3))) # F23=42.46303
F13=(((RSS1-RSS3)/(p3-p1))/(RSS3/(n-p3))) # F13=27.02593
p12=(1-pf(F12,1,108)) # p12=0.004605629
p23=(1-pf(F23,1,107)) # p23=2.423506e-09
p13=(1-pf(F13,2,107)) # p13=3.157663e-10
### From RSS, we can tell that RSS3<RSS2<RSS1, which means mod3 explains more response variables than mod2 and mod1.
### Based on p-value<0.05, three models are significantly different from one another.
### Therefore, mod3 explains significantly greater amount of response variances comparing to mod2 and to mod1
### because the probability for mod1 to explain the same amount of variances as mod3 is 3.157663e-10
### and the probability for mod2 to explain the same amount of variances as mod3 is 2.423506e-09
### And thus, mod3 can provide resonable estimate of regreassion coefficients.

# Exercise 3
AIC(mod1) # AIC(mod1)=788.9671
AIC(mod2) # AIC(mod2)=782.6779
AIC(mod3) # AIC(mod3)=747.5795
## AIC(mod1)-AIC(mod2)=6.2892
## AIC(mod2)-AIC(mod3)=35.0984
## AIC(mod1)-AIC(mod3)=41.3876
### The smaller the AIC is, the less information is being lost.
### mod3 has the smallest AIC, and the differences between mod3 and mod2, and between mod3 and mod1 are greatern than 10.
### Therefore, mod3 is significantly different from mod1 and mod2, and performs better than mod1 and mod2.