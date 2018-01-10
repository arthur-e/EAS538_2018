#####Exercise 1
##Try to use the following data set to execute ANCOVA and make interaction plots 
##This data set contains the fruit weight (Fruit) of different plants with 
##different root depth (root) and different treatment (Grazing).

library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)

mod = lm(Fruit~ Root*Grazing, data=gz)
summary(mod)
##Grazing has a significant impact on fruit weight. Root depth has a significant impact. The interaction is not significant.

library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated Coefficient for Root", title="Estimated coefficient of Root on Grazing")
#Grazing (1) has a greater effect than no grazing (0) on root depth, but the difference is not significant because it is still
#within the confidence interval of the no grazing point.

interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated Coefficient for Grazing", title="Estimated coefficient of Grazing on Root")

####Exercise 2
##1. Use F-test to compare mod1 and mod2. Explain why you chose one over another to explain the data.

##load data
data("airquality")
head(airquality, 15)

##run mod1
mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)

#F-test
anova(mod1,mod2)
#I pick mod2 because it has a smaller RSS, meaning that it explains a greater amount of the variance of the data. 
#The f-statistic is 17.085 with a significant p-value of 6.927e-05.

##2. Plot two interaction plots with interplot() for the model with wind and solar 
##radiation as the independent variables. Interprete the two interaction plots.

mod3 = lm(Temp~Wind*Solar.R, data=airquality)
summary(mod3)
#No significant interaction between solar radiation and wind because the p-value is well above .05. 

interplot(mod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar.R")
#As wind increases, the coefficient for solar radiation decreases. However, the difference is still within the confidence interval, so it is not statistically significant.
#This is another way of showing that there is no significant interaction between wind and solar radiation.

interplot(mod3, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="coefficient for Wind")
#Same as above, no significant interaction between wind and solar radiation because the difference is still within the confidence interval.