## EXERCISE 1
## Try to use the following data set to execute ANCOVA and make interaction plots This data set contains the fruit weight (Fruit) 
## of different plants with different root depth (root) and different treatment (Grazing).

library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)

str(gz)
# Root & Fruit are continuous variables, Grazing is a categorical variable with two levels.

ggplot(data=gz, mapping=aes(x=Root, y=Fruit, color=factor(Grazing)))+
  geom_point()
# By eyeballing the visualized data, there appears to be a strong positive relationship between root depth and fruit size, as 
# well as clear differences between the root depth of the grazed and ungrazed.

# Check if fruit size is normally distributed:

ggplot(data=gz, mapping=aes(x=Fruit)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")

qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"], col="red")

shapiro.test(gz[,"Fruit"])
## Shapiro-Wilk normality test
## data:  gz[, "Fruit"]
## W = 0.98247, p-value = 0.7798
## 'Fruit' follows a normal disturbution. No transformation of data necessary.

# Check if root depth is normally distributed:

ggplot(data=gz, mapping=aes(x=Root)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")

qqnorm(gz[,"Root"])
qqline(gz[,"Root"], col="red")

shapiro.test(gz[,"Root"])
## Shapiro-Wilk normality test
## data:  gz[, "Root"]
## W = 0.97786, p-value = 0.6104
## 'Root' also follows a normal distribution. No transformation of dada necessary. 

gz %>%
  filter(Grazing=="Grazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root Depth", y="Fruit Weight", title="Grazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

gz.grazed = gz %>%
  filter(Grazing=="Grazed")
summary(lm(Fruit~Root, data=gz.grazed))
## Significant postive relationship between Fruit size and Root depth in grazed treatments. p-value: 8.73e-11

gz %>%
  filter(Grazing=="Ungrazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root Depth", y="Fruit Weight", title="Ungrazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

gz.ungrazed = gz %>%
  filter(Grazing=="Ungrazed")
summary(lm(Fruit~Root, data=gz.ungrazed))
## Significant postive relationship between Fruit size and Root depth in ungrazed treatments. p-value: 4.715e-12


# ANCOVA
mod.gz = lm(Fruit~ Root*Grazing, data=gz)
summary(mod.gz)

## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          -125.173     12.811  -9.771 1.15e-11 ***
##  Root                   23.240      1.531  15.182  < 2e-16 ***
##  GrazingUngrazed        30.806     16.842   1.829   0.0757 .  
## Root:GrazingUngrazed    0.756      2.354   0.321   0.7500    
## ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 6.831 on 36 degrees of freedom
## Multiple R-squared:  0.9293,	Adjusted R-squared:  0.9234 
## F-statistic: 157.6 on 3 and 36 DF,  p-value: < 2.2e-16

## Interpretation: For per unit increase in root depth, the model shows that fruit size increases by 23.24 units in the grazed 
## treatments. For ungrazed treatments, fruit size increases by 23.24+0.756=23.99 units in the linear model. However, this is not
## a significant difference compared to the grazed treatments (p=0.75). 

#Interaction Plot
interplot(m=mod.gz, var1="Root", var2="Grazing")+
  labs(x="Grazing Treatment", y="Estimated coefficient for Root Depth", title="Estimated Coefficient of Root Depth on Grazing")
## Intepretation: From the resulting plot, we can see that the mean estimated coefficient for the Grazed treatment lies within 
## the confidence interval of the ungrazed treatments. The coefficients are therefore not significantly different from each 
## other.

interplot(m=mod.gz, var1="Grazing", var2="Root")+
  labs(x="Root depth", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Root Depth")
## Interpretation: The plot suggests that coefficient of fruit size for the ungrazed treatments becomes greater as root depth 
## increases. However, this increase is not significant. 


# EXERCISE 2

data("airquality")
head(airquality, 15)

mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)

# 1. Use F-test to compare mod.1 and mod.2. Explain why you chose one over another to explain the data.

anova(mod1, mod2)
## Analysis of Variance Table
## Model 1: Temp ~ Wind + Ozone
## Model 2: Temp ~ Wind * Ozone
##    Res.Df  RSS   Df  Sum of Sq  F    Pr(>F)    
## 1    113 5166.5                                  
## 2    112 4482.7  1    683.81 17.085  ***

## Interpretation: Based on the results of the f-test, Wind*Ozone is the better model to select as it significantly 
## explains more variance. The RSS is much lower, 4482.7 compared to 5166.5. The p-value of the f-statistic is 6.927e-05, 
## indicating that the probability for model 1 to explain the same amount of variance as model 2 is very low.

# 2. Plot two interaction plots with interplot() for the model with wind and solar radiation as the independent variables. 
# Interprete the two interaction plots.

mod3 = lm(Temp~Wind*Solar.R, data=airquality)
summary(mod3)
## Based on summary table, there does not seem to be significant interaction between wind and solar radiation (p=0.7537).

interplot(mod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar Radiation")
# Interpretation: The plot suggests that the coefficient for solar radiation decreases as wind increases. This, however, is
# not a significant decrease.

interplot(mod3, var1="Solar.R", var2="Wind")+
  labs(x="Solar R.", y="coefficient for Wind")
# Interpretation: The plot suggests that the coefficient for wind decreases slightly as solar radiation increases. This 
# decrease is not significant, however. 


