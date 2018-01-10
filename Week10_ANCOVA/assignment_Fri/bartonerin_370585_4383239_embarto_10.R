#Erin Barton
#19 March 2017

#Lab 8 Exercises


#Ex1
#Try to use the following data set to execute ANCOVA and make interaction plots
#This data set contains the fruit weight (Fruit) of different plants with 
#different root depth (root) and different treatment (Grazing).

library(Lahman)
library(dplyr)
library(ggplot2)
library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)

#data exploration
str(gz)
ggplot(data=gz, mapping=aes(x=Root, y=Fruit, color=factor(Grazing)))+geom_point()
ggplot(data=gz, mapping=aes(x=Root))+geom_histogram(aes(y=..density..))+geom_density(color="red")
qqnorm(gz[,"Root"])
qqline(gz[,"Root"],col="red")
shapiro.test(gz[,"Root"])
ggplot(data=gz, mapping=aes(x=Fruit))+geom_histogram(aes(y=..density..))+geom_density(color="red")
qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"], col="red")
shapiro.test(gz[,"Fruit"])

gz %>%
  filter(Grazing=="Ungrazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root Depth", y="Fruit Weigth", title="Ungrazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)
gz.Ungrazed = gz %>%
  filter(Grazing=="Ungrazed")
summary(lm(Fruit~Root, data=gz.Ungrazed))
#Call:
#  lm(formula = Fruit ~ Root, data = gz.Ungrazed)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-9.4542 -4.2430 -0.4643  4.8578  8.8639 
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -94.367      9.211  -10.24 6.14e-09 ***
#  Root        23.996      1.507   15.93 4.72e-12 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.755 on 18 degrees of freedom
#Multiple R-squared:  0.9337,	Adjusted R-squared:   0.93 
#F-statistic: 253.6 on 1 and 18 DF,  p-value: 4.715e-12

gz %>%
  filter(Grazing=="Grazed") %>%
  ggplot(aes(x=Root, y=Fruit))+
  geom_point(color="blue")+
  labs(x="Root", y="Fruit", title="Grazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)
gz.Grazed = gz %>%
  filter(Grazing=="Grazed")
summary(lm(Fruit~Root, data=gz.Grazed))
#Call:
#  lm(formula = Fruit ~ Root, data = gz.Grazed)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-17.3177  -1.7272   0.6796   2.6748  17.1313 

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -125.173     14.552  -8.602 8.59e-08 ***
#  Root       23.240      1.739  13.366 8.73e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7.759 on 18 degrees of freedom
#Multiple R-squared:  0.9085,	Adjusted R-squared:  0.9034 
#F-statistic: 178.7 on 1 and 18 DF,  p-value: 8.73e-11

#These data explorations all show that both the root depth and the fruit weight are normally 
#distributed. The two variables also vary according to whether the area has been grazed or not,
#although the slopes of the lines are very similar, suggesting there may not be a large effect 
#by grazing. 

#ANCOVA Model 
mod <- lm(Fruit~Root*Grazing, data=gz)
summary(mod)

#Call:
#lm(formula = Fruit ~ Root * Grazing, data = gz)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-17.3177  -2.8320   0.1247   3.8511  17.1313 

#Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          -125.173     12.811  -9.771 1.15e-11 ***
#  Root                23.240      1.531  15.182  < 2e-16 ***
#  GrazingUngrazed     30.806     16.842   1.829   0.0757 .  
#Root:GrazingUngrazed   0.756      2.354   0.321   0.7500    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 6.831 on 36 degrees of freedom
#Multiple R-squared:  0.9293,	Adjusted R-squared:  0.9234 
#F-statistic: 157.6 on 3 and 36 DF,  p-value: < 2.2e-16

#This summary of the ANCOVA mmodel shows that the slope of the line describing the effect of root depth on fruit in grazed areas is 23.24. This slope is not significantly different
#from the slope of the line describing the effect of root depth on fruit in ungrazed areas, nor is the different between the line intercepts significantly different. Thus grazing
#does not seem to have a large effect on the relationship between root depth and fruit weight. 


# ---

#Exercise 2[Bonus 5 pt ] 
#Use F-test to compare mod.1 and mod.2. Explain why you chose one over another to explain 
#the data.

install.packages("interplot")
library(interplot)
data("airquality")
head(airquality, 15)
airquality <- na.omit(airquality)

#Models
mod = lm(Temp~Wind, data=airquality)
summary(mod)
plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")
mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)
interplot(mod2, var1="Wind", var2="Ozone")+
  labs(x="Wind", y="coefficient for Ozone")

#Compare mod1 and mod2
anova(mod1, mod2)
#Analysis of Variance Table

#Model 1: Temp ~ Wind + Ozone
#Model 2: Temp ~ Wind * Ozone
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1    108 5038.5                                  
#2    107 4362.9  1    675.59 16.569 9.009e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#This output suggest that mod2 is a better model. This is because the p-value shows that 
#the probability for mod1 to explain the same amount of variance as mod2 is very low, 9.009e-05. 

#Interaction Plots: Plot two interaction plots with interplot() for the model with wind and 
#solar radiation as the independent variables. Interprete the two interaction plots.
mod3 <- lm(Temp~Wind * Solar.R, data=airquality)
summary(mod3)
#There does not seem to be a significant interaction between Wind and Solar.R. 
interplot(mod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar.R")
interplot(mod3, var1="Solar.R", var2="Wind")+
  labs(x="Wind", y="coefficient for Wind")
#These two interaction plots both show a horizontal line, indicating that there is no significant interaction between wind and solar radiation.
