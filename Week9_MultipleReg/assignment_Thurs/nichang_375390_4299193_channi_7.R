setwd("D:/UM/SNRE/Winter 2017/NRE 538 Statistics/Week 9 Lab")
library(dplyr)
airquality1 = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

#Exercise 1 Manipulate the above code to reduce the correlation between X1 and X2
#and recalculate the regression coefficients. Explain what you find.
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0, max=10) #Increase the noise
X2 = (Y+3)/2 + runif(length(Y), min=0, max=10)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))
plot(X1,X2)
cor(X1,X2)

#The correlation coefficients of X1 and X2 has reduced to 0.3-0.4. The plot also confirmed the reduced correlation.
#As X1 and X2 become less correlated, the outcome of multiple regrssion deviate less from the outcomes of separate regression models.
#However, the increae in noise also caused a decrease in the regression coefficient of Y on X1 and X2.
#Both values dropped to around 1. The noise reduced to accuracy of the model as well

#Exercise 2 
mod1=lm(Temp~Wind,data=airquality1)
summary(mod1)
mod2=lm(Temp~Wind+Solar.R,data=airquality1)
summary(mod2)
#Bonus: Challenge yourself by calculateing the RSS of the two models, F statistics and the p-value by yourself
res1=residuals(mod1)
RSS1=sum(res1^2)
res2=residuals(mod2)
RSS2=sum(res2^2)
Fvalue=(RSS1-RSS2)/(RSS2/108) #p2-p1=1, n-p2=108
Fvalue #8.374, same withe result of anova
pvalue=1-pf(Fvalue,1,108) #0.0046,same withe result of anova
anova(mod1,mod2)

#Compare a third model (mod3) with ozone varaible as the third independent variable to mod1 and mod2 above. Does it explain more variance? 
#Does it give you reasonable estimate of regression coefficients?
mod3=lm(Temp~Wind+Solar.R+Ozone,data=airquality1)
summary(mod3)
anova(mod1,mod3)
#The p-value<0.05. Mod3 explain significantly greater amount of variance than mod1.
#If the two models explain the same amount of variance, the chance we would observe the difference in sum of squares (2524.1)is 3.16e-10
anova(mod2,mod3)
#The p-value<0.05. Mod3 explain significantly greater amount of variance than mod1.
#If the two models explain the same amount of variance, the chance we would observe the difference in sum of squares (1982.9)is 2.42e-09

cor(airquality1[,c(1:4)])
summary(lm(Solar.R~Ozone,data=airquality1))
summary(lm(Wind~Ozone,data=airquality1))
#The estimation of regression coefficients in mod3 may not be so accurate, as wind and solar radiaton
#are both significantly correlated to ozone.

#Exercise 3 Compare the AIC of the three models above mod1, mod2, and mod3. Do you have different conclusion in terms of which model performs better?
AIC(mod1) #788.97
AIC(mod2) #782.68
AIC(mod3) #747.58
#mod3 loses significantly less information than mod1 and mod2.
#Conservatively (taking AIC differnce>10 as being significantly different), the performance 
#of mod1 and mod2 are not significantly different, although there seem to be some difference.