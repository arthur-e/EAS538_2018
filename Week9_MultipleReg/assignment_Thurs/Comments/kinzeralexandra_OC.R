data("airquality")
head(airquality, 15)
install.packages(dplyr)
library(dplyr)

##remove NAs
airquality = airquality %>%   
subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
airquality

###EXERCISE 1###

##Manipulate the above code to reduce the correlation between X1 and X2 and recalculate the regression coefficients. 
##Explain what you find.

##hint: Increase the random noise of X1X1 and X2X2 so that the 
##two can be less correlated.

#Original R^2 = 0.9641431

Y = seq(from=0, to=20, by=0.01)
X1 = (Y-2)/3 + runif(length(Y), min=10, max=20)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=1)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))

summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))
plot(X1~X2)

cor(X1, X2) # 0.5441585

#I changed the max value in the runif() function to increase the noise.
#This creates more variation in the data points that leads to a lower correlation coefficient (R^2).




###EXERCISE 2###
##[Bonus, 2 pts] Challenge yourself by calculateing the RSS of the two models, 
##FF statistics (1 pt) and the p-value (1 pt) all by yourself (i.e. not using the anova() function)

mod1 = lm(Temp~Wind, data=airquality)
summary(mod1) #R^2 = ~.25
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2) #R^2 = ~.3

#RSS
res1 = residuals(mod1)
RSS1 = sum(res1^2)
RSS1 #7520.672
anova(mod2) ##confirm RSS1

res2 = residuals(mod2)
RSS2 = sum(res2^2)
RSS2 #6979.506
anova(mod2) ##confirm RSS2


#calculate f-stat manually
Fstat = (RSS1-RSS2/(3-2)) / (RSS2/(summary(mod2)$df[2])) #using formula from lab html page
Fstat #8.373933

#calculate p-value manually
1-pf(Fstat, df1=1, df2=nrow(airquality)-3) ## 0.004605629

#Confirm values with anova
anova(mod1,mod2)
##Results
##Analysis of Variance Table

##Model 1: Temp ~ Wind
##Model 2: Temp ~ Wind + Solar.R
##Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
##1    109 7520.7                                
##2    108 6979.5  1    541.17 8.3739 0.004606 **
  ---
##  Signif. codes:  0 ?€?***?€? 0.001 ?€?**?€? 0.01 ?€?*?€? 0.05 ?€?.?€? 0.1 ?€? ?€? 1
# Results for manual calculations and anova are the same! Woohoo!

  

#Compare a third model (mod3) with ozone variable as the third independent variable to mod1 and mod2. 
#Does it explain more variance? Does it give you reasonable estimate of regression coefficients?
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3) ##R^2 = ~.5

##According to the R2 (~.5 for mod3, vs. ~.3 for mod2 and ~.25 for mod1), mod3 does explain more variance.
##The summary table suggests that Ozone more strongly explains the variance in temperature than Wind and Solar Radiation
##because those 2 variables are no longer significant (based on p-value) in mod3. Ozone is signifacant in explaining the variance according to the p-value.

#####Oscar: When wind and solar radiation becomes not significant after including ozone, it does not mean ozone explain more than the two. 
##### It only shows the correlation among ozone and theose two variables. 

##Does it give a more reasonable estimate of regression coefficients? 
anova(mod1,mod2,mod3)
##Because mod3 has the smallest RSS, it is the best model. There is a smaller chance of finding a model that 
#better explains the variance in Temperature with mod3 than mod2 or mod1.



###EXERCISE 3###
###Compare the AIC of the three models above mod1, mod2, and mod3. 
###Do you have different conclusion in terms of which model performs better?

AIC(mod1) ##788.9671
AIC(mod2) ##782.6779
AIC(mod3) ##747.5795

##The AIC value goes down significantly (>10) in both mod1 and mod2 compared with mod 3, suggesting that there is less information lost in mod 3 than in both mod1 and mod2.
##The difference between mod1 and mod2 is 6.29, which does not suggest a clear significant difference. Generally speaking, we could say that the amount of information lost by the
##models decreases as we add more explanatory variables. Mod3 wins again!
