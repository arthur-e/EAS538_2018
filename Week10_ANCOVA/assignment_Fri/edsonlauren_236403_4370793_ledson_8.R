##Lauren Edson
## NRE 538 Lab assignment 8

install.packages("RCurl")
install.packages("ggplot2")
install.packages("interplot")

library(ggplot2)
library(RCurl)
library(interplot)
library(stringi)

gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
str(gz)

gz %>%
  filter(Grazing=="Ungrazed") %>%
  ggplot(aes(x=Root, y=Fruit))+
  geom_point(color="blue")+
  labs(x="Root", y="Fruit", title="Ungrazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

gz %>%
  filter(Grazing=="Grazed") %>%
  ggplot(aes(x=Root, y=Fruit))+
  geom_point(color="blue")+
  labs(x="Root", y="Fruit", title="Grazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

##################
##Exercise 1
# Try to use the following data set to execute ANCOVA and make interaction plots 

fruitmod = lm(Fruit~ Root*Grazing, data=gz)
fruitaov = aov(Fruit~ Root*Grazing, data=gz)
summary(fruitmod)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -125.173     12.811  -9.771 1.15e-11 ***
#   Root                   23.240      1.531  15.182  < 2e-16 ***
#   GrazingUngrazed        30.806     16.842   1.829   0.0757 .  
# Root:GrazingUngrazed    0.756      2.354   0.321   0.7500  
# Multiple R-squared:  0.9293,	Adjusted R-squared:  0.9234

#the output tells us that Fruit is -125 when root is 0. The slope of the first line 
#(which refers for the grazed data points) is 23.2, meaning one unit of root changes 
#fruit by 23 units. The line that shows the ungrazed data points has a slope of 30.8, 
#meaning that one unit of root changes fruit by 30.8. The Interaction term in this model
#is 0.75, which means that there is a difference of 0.75 between the slopes as root increases.
#the P-values show that the intercept is significant, as is the slope of the grazed line, but 
#the slope of the ungrazed line is not significantly different that the grazed line. R^2
#tells us that this is a good fitting model


anova(fruitmod)
#the anova output shows that the p-values are significant for Root, and Grazing
#This also reports the sum of squares to allow for comparing models

summary(fruitaov)
#Also outputs p-values for checking significance and sum of squares


interplot(m=fruitmod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Grazing")
#The interplot shows that the effect is not significant, but ungrazed is slightly higher.

interplot(m=fruitmod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Root")
#When Root is low, the the effect of ungrazed on fruit is lower than when root is higher.
#The plot shows the differences in effects between grazed and ungrazed.





##################
##Exercise 2

data("airquality")
head(airquality, 15)

mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)

interplot(mod2, var1="Wind", var2="Ozone")+
  labs(x="Wind", y="coefficient for Ozone")

interplot(mod2, var1="Ozone", var2="Wind")+
  labs(x="Ozone", y="coefficient for Wind")

###############
#1. Use F-test to compare mod.1 and mod.2. Explain why you chose one over another to 
#explain the data.

anova(mod1,mod2)
# Analysis of Variance Table
# Model 1: Temp ~ Wind + Ozone
# Model 2: Temp ~ Wind * Ozone
#   Res.Df  RSS Df Sum of Sq      F    Pr(>F)    
# 1    113 5166.5                                  
# 2    112 4482.7  1    683.81 17.085 6.927e-05 ***
##Based on running the F-test on these nested models, the p-value
#tells us that the second model, mod2 with the interaction term, is the better
#model for describing the variation in temperature based on wind and ozone.


##################
#2. Plot two interaction plots with interplot() for the model with wind and solar 
#radiation as the independent variables. Interprete the two interaction plots.

tempmod = lm(Temp~Wind*Solar.R, data=airquality)
summary(tempmod)

interplot(tempmod, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar.R")
#This plot shows a slightly negative relationship between wind and the effect of solar.R
#At low wind speeds, the effect of solar radiation is about -1.0. When Wind speeds is high,
#the effect of solar radiation is about -1.5.


interplot(tempmod, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="coefficient for Wind")
#This plot also shows a negative trend for the effect of wind on temperatures over changing
#solar raditation. At low solar radiation, the effect of wind is 0.030. At high solar radiation,
# the effect of wind on temperature is 0.020.
