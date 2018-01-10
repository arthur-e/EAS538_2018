## EXERCISE 1
## Manipulate the above code to reduce the correlation between X1 and X2 and recalculate the regression coefficients. 
## Explain what you find.
## hint: Increase the random noise of X1 and X2 so that the two can be less correlated.

Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+3)/2 + runif(length(Y), min=3, max=30)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))

summary(lm(Y~X2, data=df))

plot(X1~X2)

cor(X1,X2)

#New correlation coefficient after increasing noise = 0.389, comapred to 0.966.


## EXCERISE 2
## [Bonus, 2 pts] Challenge yourself by calculateing the RSS of the two models, F statistics (1 pt) and the p-value (1 pt) 
## all by yourself (i.e. not using the anova() function)

mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)

res.mod1 = residuals(mod1)
res.mod2 = residuals(mod2)

RSS.mod1 = sum(res.mod1^2)
RSS.mod2 = sum(res.mod2^2)

F.stat = ((RSS.mod1 - RSS.mod2)/(2-1))/(RSS.mod2/(110-2)) #F-statistic: 8.374

?pf

pf(q=F.stat, df1=1, df2=108, lower.tail=FALSE) #p-value: 0.004606

anova(mod1, mod2) #Check with ANOVA

## Compare a third model (mod3) with ozone varaible as the third independent variable to mod1 and mod2 above. Does it explain 
## more variance? Does it give you reasonable estimate of regression coefficients?

mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)

# Adding the Ozone factor dramatically changed the regression coefficients of both wind and radiation, suggesting that the three
# factors are not independent. The regression coefficients may not be good estimates.

pairs(airquality)
cor(airquality) # wind & ozone: -0.612496576 ; temp & ozone: 0.698541410 ; correlation coefficients for both are > 0.5

vif.oz = 1/(1 - summary(lm(Ozone~Wind+Solar.R, data=airquality))$r.squared)
vif.oz # = 1.8165

## VIF still < 5.  Inflation factor may still be within reasonable limits.

anova(mod1, mod2, mod3)
##Analysis of Variance Table
##Model 1: Temp ~ Wind
##Model 2: Temp ~ Wind + Solar.R
##Model 3: Temp ~ Wind + Solar.R + Ozone
##Res.Df    RSS   Df  Sum of Sq    F    Pr(>F)    
##1    109 7520.7                                  
##2    108 6979.5  1    541.17 11.589 0.0009349 ***
##3    107 4996.6  1   1982.90 42.463 2.424e-09 ***

## Model 3 explains significantly more variance compared to both model 2 and model 1. p-value: 2.424e-09


## EXERCISE 3
## Compare the AIC of the three models above mod1, mod2, and mod3. Do you have different conclusion in terms of which 
## model performs better?

AIC(mod1, mod2, mod3)
##      df    AIC
## mod1  3 788.9671
## mod2  4 782.6779
## mod3  5 747.5795

## Comparing between the AIC values, there is not a significant difference between the performance of models 1 and 2 
## though model 2 is slightly better (AIC value difference between 1 & 2 = 6.2892, which is less than 10). In contrast, the 
## F-test suggested that model 2 significantly explained more variance based on the low p-value (0.0046). 
## Based on AIC, Model 3 appears to be a better model than 1 & 2 (AIC value difference between 2 & 3 = 35.0984). With the 
## F-test, we came to a similar conclusion with a very low p-value (2.424e-09). 
















