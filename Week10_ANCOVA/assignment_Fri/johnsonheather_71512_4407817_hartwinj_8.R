#Exercise 1

gz=read.table("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt", sep="", header=T,comment.char="#")            
head(gz,100)
mod=lm(Fruit~Root*Grazing, data=gz)
aov=aov(Fruit~Root*Grazing, data=gz)
summary(mod)
anova(mod)
library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+labs(x="Fruit Weight", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Fruit Weight")
interplot(m=mod, var1="Grazing", var2="Root")+labs(x="Root", y="Estimated coefficient for Fruit Weight", title="Estimated Coefficient of Grazing on Fruit Weight")

#Exercise 2
data("airquality")
head(airquality, 15)
mod = lm(Temp~Wind, data=airquality)
summary(mod)
plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")
mod1=lm(Temp~Wind+Ozone, data=airquality)
summary(mod1)
mod2=lm(Temp~Wind*Ozone, data=airquality)
summary(mod2)
interplot(mod2, var1="Wind", var2="Ozone")+labs(x="Wind", y="Coefficient for Ozone")
interplot(mod2, var1="Ozone", var2="Wind")+labs(x="Ozone", y="Coefficient for Wind")

#Part 1.

anova(mod1,mod2)

#mod 2, the interaction of wind and ozone has a greater effect on temperature and is a better model.
#The RSS of mod1 is 5166.6 while the RSS of mod2 is 4482.7.  The F-stat is 17.085, p-value is 6.927e-05;
#mod2 explains significantly greater amount of variance compared to mod1. The probability for mod1 to
#explain the same amount of variance as mod 2 is 6.927e-05.  In other words, the probability of mod1
#explaining the same amount of variance is really really really small!

#Part 2.

mod3=lm(Temp~Wind+Solar.R, data=airquality)
summary(mod3)
mod4=lm(Temp~Wind*Solar.R, data=airquality)
summary(mod4)
interplot(mod4,var1="Wind", var2 = "Solar.R")+labs(x="Wind", y="Coefficient for Solar Radiation")
interplot(mod4, var1="Solar.R", var2="Wind")+labs(x="Solar.R", y="Coefficient for Wind")

#Both interaction plots show that one independent variable depends on the other;  
#where the y-axis is the coefficient for Solar Radiation:  With increasing wind, 
#the magnitude of the coefficient for Solar Radiation decreases.
#Where the y-axis is the Coefficient for Wind, increasing Solar Radiation has a negative 
#effect on the magnitude of the wind (wind coefficient decreases).