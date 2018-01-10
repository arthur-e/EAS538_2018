data("airquality")
head(airquality,15)
library(dplyr)
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2,data = df))
plot(X1~X2)
cor(X1, X2) #0.9607188
####Exercise1####
Y = seq(from=0, to=20, by=0.1)
X3=(Y-2)/3+runif(length(Y),min=0,max=20)
X4=(Y+3)/2+runif(length(Y),min=0.1,max=5)
summary(lm(Y~X3, data=df))
summary(lm(Y~X4, data=df))
summary(lm(Y~X3+X4,data = df))
plot(X3~X4)
cor(X3, X4) #0.2748938
#When I change the noise, the correlation value between X3 and X4 become smaller.Points are more scattered
####



####Exercise2####
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)
anova(mod1,mod2)
res1=residuals(mod1)
RSS1=sum(res1^2)
RSS1 #7520.672
res2=residuals(mod2)
RSS2=sum(res2^2)
RSS2 #6979.506
Fvalue=((RSS1-RSS2)/(3-2))/(RSS2/(111-3))
Fvalue #8.373933
Pvalue=pf(8.374,1,108,lower.tail=FALSE)
Pvalue  #0.004605471


mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod2,mod3)
#pvalue=2.424e-09,so mod3 explains significantly greater amount of variance compare to mod 2. 
#The probablity for mod2 to explain the same amount of variance as mod3 is 2.424e-09
res3=residuals(mod3)
RSS3=sum(res3^2)
RSS3 #4996.601
Fstatistic=((RSS2-RSS3)/(4-3))/(RSS3/(111-4))
Fstatistic #42.463
p=pf(42.463,1,107,lower.tail=F)
p #2.423528e-09
  
####Exercise3####
AIC1=AIC(mod1)
AIC1 #788.9671
AIC2=AIC(mod2)
AIC2 #782.6779
AIC3=AIC(mod3)
AIC3 #747.5795
#mod3 can loss least information, so mod3 is the best model.