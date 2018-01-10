library(Lahman)
library(dplyr)
library(ggplot2)

###Exercise 1
gz = read.table(file = "https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt", sep="", header=T, comment.char="#")

shapiro.test(gz[,"Root"]) #Since this has a p value > .05 we can reject the null hypothesis that this variable is not normally distributed. Therefore we can assume that the data are normally distributed.
shapiro.test(gz[,"Fruit"]) #Since this has a p value > .05 we can reject the null hypothesis that this variable is not normally distributed. Therefore we can assume that the data are normally distributed.

mod = lm(Fruit ~ Root*Grazing, data=gz)
summary(mod)
#This is saying that each additional unit of roots adds 23.240 units of weight to the plant.
#Plants with the Ungrazed treatments have 30.806 more units of weight than plants with Grazed treatments.
#Roots with Ungrazed treatments have .756 more units of weight than roots with Grazed treatments.

anova(mod)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="type of grazing treatment", y="estimated coefficient of length of root", title="Estimated Coefficient of Length of Root on Grazing Type")
#We can see from the anova() function and the chart that the root length has more of an effect on the weight of fruit when the ungrazed treatment is used versus the grazed treatment.
#But the anova() function shows us that the effect is not significantly different.
interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="root length", y="estimated coefficient of grazing treatment", title="Estimated coefficient of grazing treatment on root length")
#There is a positive effect between fruit weight and changing the grazing treatment to ungrazed. The effect becomes stronger as root length increases.
#However, as above, this is not a significant effect (as can be seen with the anova() function)

###Exercise 2
#Part 1
mod = lm(Temp~Wind + Ozone, data=airquality)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
anova(mod, mod2)
#Since the p-value from the anova() function is less than .05, we know that Model 1 is significantly different from Model 2.
#I would choose to use Model 2 because the RSS value for Model 2 is smaller than the RSS value of Model 1.
#This means that Model 2 explains more of the variance.

#Part 2
mod3 = lm(Temp~Wind*Solar.R, data=airquality)
interplot(m=mod3, var1="Solar.R", var2="Wind")+
  labs(x="Wind", y="Estimated coefficient of solar radiation", title="Estimated coefficient of radiation on wind")
#As wind increases, the positive effect of a change in radiation on temperature decreases. But this change isn't significant.