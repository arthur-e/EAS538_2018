##Excersize One##
install.packages("RCurl")
library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
mod=lm(Fruit~ Root*Grazing, data=gz)
summary(mod)
anova(mod)
##Or can use aov()
aov = aov(Fruit~Root*Grazing, data=gz)
summary(aov)
##The interaction term (0.75) is not significant, which means the effect of one
##variable does not depend on the other one. This means that the effect of root depth on
##fruit weight is not significantly different based on grazing treatment.

#The intercept is the fruit weight of the grazing treatment (alphabetical) when root depth is zero.
#The estimate of Root means the effects of root depth on fruit weight in grazing treatment. 
#The estimate of grazing treatment is the average fruit weight difference between the two treatments.
#The interaction term means whether the effect of root depth differs from grazing treatment to nongrazing treatment

install.packages("interplot")
install.packages('ggplot2')
library(ggplot2)
library(interplot)
interplot(m=mod, var1= "Root", var2="Grazing")+labs(x="Treatment", y="Estimated Coefficient for Root Depth", title="Estinated Coefficient of Root Depth on Treatment")
#Plot shows that the effect of root depth on Fruit Weight is higher in the ungrazed treatment
#than the grazed treatment. However the difference is not significant.
interplot(m=mod, var1="Grazing", var2="Root")+labs(x="Root", y="Estimated coefficient for Treatment", title="Estimated Coefficient of Treatment on Root Depth")
#Plot shows that the effect of root depth on fruit weight is at first lower for grazing
#treatment, and gradually the effect gets greater for the ungrazed treatment. 
#However, the difference is not significant.


##Excersize Two
#1
mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)
#Since the models are nested, we can use anova() to perform the f-test
anova(mod1, mod2)
#The RSS of mod 1 is 5166.5 and the RSS of mod2 is 4482.7
#The F statistic is 17.085
#mod2 explains a greater amount of variance as compared to mod1, because
#the probability for mod1 to explain the same amount of variance as mod2
#is 0.004606. Thus, I would choose mod2 over mod1.

#2
mod3= lm(Temp~Wind*Solar.R, data=airquality)
summary(mod3)
#Insignificant interaction term
interplot(m=mod3, var1="Wind", var2="Solar.R")+
  labs(x="Solar Radiation", y="Estimated coefficient for Wind", title="Estimated Coefficient of Wind on Solar Radiation")
#This graph shows that the effect of wind on temperature decreases as solar radiation decreases,
#however the difference is not significant
interplot(mod3, var1="Solar.R", var2="Wind")+labs(x="Solar Radiation", y="coefficient for Wind")
##This graph shows that the effect on solar radiation on tempearture decreases as wind decreases, 
#however this difference is not significant.