
####EXERCISE 1:

install.packages("RCurl")
library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")

mod1 = lm(Fruit~Root*Grazing, data=gz)
aov1 = aov(Fruit~Root*Grazing, data=gz)
summary(mod1)
anova(mod1)
summary(aov1) 

install.packages("interplot")
library(interplot)
interplot(m=mod1, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Grazing")
interplot(m=mod1, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Graing on Root")

#in the first plot, since the error bars of grazing and ungrazed overlap, there is no significant difference between the effect size of grazing on root
#the effect of grazing on fruit at low root values is less than the effect of grazing on fruit at high root values


####NOTES AGAIN
data("airquality")
head(airquality, 15)

airqualmod = lm(Temp~Wind, data=airquality)
summary(airqualmod)

plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")

airqualmod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(airqualmod1)

airqualmod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(airqualmod2)

#interplot

interplot(airqualmod2, var1="Wind", var2="Ozone")+
  labs(x="Wind", y="coefficient for Ozone")

interplot(airqualmod2, var1="Ozone", var2="Wind")+
  labs(x="Ozone", y="coefficient for Wind")



###EXERCISE 2
data("airquality")
head(airquality, 15)

airqualmod = lm(Temp~Wind, data=airquality)
summary(airqualmod)

plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")

airqualmod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(airqualmod1)

#wind, ozone, and interaction
airqualmod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(airqualmod2)

anova(airqualmod1,airqualmod2)

#F-test shows that the models are significantly different
#Airqualmod2 has a lower RSS, so is a better fit
#Because the adjusted R squared value is higher for airqualmod2, we can see that airqualmod2 explains more variation in the data.
##So, I would choose Airqualmod2 to explain the data.

airqualmod3 = lm(Temp~Wind*Solar.R, data=airquality)
summary(airqualmod3)

interplot(airqualmod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar.R")

interplot(airqualmod3, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="coefficient for Wind")

#As wind increases, the effect of Solar.R slightly decreases; as wind decreases, the effect of Solar.R increases 
#As Solar.R increases, the effect of wind slightly decreases; as Solar. R decreases, the effect of wind slightly increases
#However, the interaction between the variables is not significant (from summary table)