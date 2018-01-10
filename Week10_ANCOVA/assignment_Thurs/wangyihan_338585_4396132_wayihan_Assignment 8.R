#Exercise 1-Assignment 8
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
mod=lm(Fruit~Root*Grazing, data=gz)
aov=aov(Fruit~Root*Grazing, data=gz)
summary(mod)
summary(aov)
install.packages("interplot")
library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Grazing")
interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Roots")

#Exercise 2
#Question 1
data("airquality")
head(airquality, 15)
mod1=lm(Temp~Wind+Ozone, data=airquality)
mod2=lm(Temp~Wind*Ozone, data=airquality)

aov1=aov(Temp~Wind+Ozone, data=airquality)
aov2=aov(Temp~Wind*Ozone, data=airquality)
summary(aov1)
summary(aov2)

anova(mod1,mod2)
#Anova summary table shows p value equals to 6.927e-05, which means there is significant difference
#between mod1 and mod2. (mod2 is nested in mod1)
#The RSS of mod2 is smaller than that of mod1, which shows that the mod2 represent the sample better than mod2. 
#Plus, mod2 explains interaction between wind and ozone, from which we can tell whether the effect of Wind on Temperature differ from Ozone.

#Question 2
library(interplot)
mod=lm(Temp~Wind*Solar.R, data=airquality)
interplot(m=mod, var1="Wind", var2 = "Solar.R")+
  labs(x="Wind",y="Estimated coefficient for Wind", title="Estimated Coefficient of Wind on Solar.R")
interplot(m=mod, var1="Solar.R", var2 = "Wind")+
  labs(x="Solar.R",y="Estimated coefficient for Solar.R", title="Estimated Coefficient of Solar.R on Wind")
#The interplots show that the coefficient of Wind is not significantly depending on Solar rediation, and vice versa. 
#Because we can draw a horizontal line within the shade area. The shade area is not significanlty increasing or decreasing from left side to the right. 
