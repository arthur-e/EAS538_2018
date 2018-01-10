##Exercise 1####
gz = read.table("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt", sep="", header=T,comment.char="#")
head(gz)
qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"],col="red")
shapiro.test(gz[,"Fruit"])
#The p-value of shapiro test is 0.7798>0.05, so the weight data is normally distributed

qqnorm(gz[,"Root"])
qqline(gz[,"Root"],col="red")
shapiro.test(gz[,"Root"])
#The p-value of shapiro test is 0.6104>0.05, so the Root data is normally distributed

mod0=lm(Fruit~Root*Grazing,data=gz)
summary(mod0)
#The intercept is -125.173, the pvalue is 1.15e-11. The intercept is significant different from 0. 
#The estimate in second row is 23.240, pvalue<2e-16. This means,root depth has a significant affect to weight of fruit.
#The estimate in third row is 30.806, pvalue is 0.0757. This means, the difference of fruit weight between grazed and ungrazed group is 30.806, but this difference is not significantly difference.
#The estimate in the fourth row is 0.756, the pvalue is 0.75. This means, the the difference of the root depth's regression coefficient value in grazed group and ungrazed group is 0.756, but this difference is not significant
#The adjusted R square is 0.9234, and pvalue<2.2e-16. So, this is a good model, this model can explain a most portion of the data. 
#In this model, root depth has a significant effect on fruit weight, but grazeing has no significant effect on fruit weight.

install.packages("interplot")
library(interplot)

interplot(m=mod0, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for root depth", title="Estimated Coefficient of root depth on grazing")
#The effect of root depth on fruit weight is higher in ungrazed group, but the difference is not significant

interplot(m=mod0, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for grazing", title="Estimated Coefficient of grazing on root depth")
#The coefficient of ungrazed is higher than grazed group, and the difference get larger when root depth increase. But this difference is not significant

#Exercise 2####
data("airquality")
head(airquality, 15)
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
interplot(mod2, var1="Ozone", var2="Wind")+
  labs(x="Ozone", y="coefficient for Wind")

#1
anova(mod1,mod2)
#Since pvalue is smaller than 0.05, so mod1 and mod2 are significantly differet from each other. 
#I will select mod2, because the summary of mod2 shows wind and Ozone have interactive effect with each other (the p value of "wind: Ozone" is smaller than 0.05), 
#and the F-test shows the probability that mod1 can explain mod2 is just 6.927e-05. 
#From summary of mod1 and mod2, we can find that the adjusted R squared of mod1(0.4918), is lower than adjusted R squared of mod2(0.5552)
#this also shows, mod2 can explain more part of our data than mod1. 

#2
mod3=lm(Temp~Wind*Solar.R, data=airquality)
summary(mod3)
#The summary shows wind has significant effect on temperature (p=0.0142), solar radiation cannot significantly affect temprature (p=0.1718), 
#wind and solar radiation are not interactive (p=0.7537),which means solar radiation's change cannot siginificantly affect how wind affect temperature.
interplot(mod3,var1="Wind",var2="Solar.R")+
  labs(x="Wind",y="coefficient for solar radiation")
#When the value of wind become higher, the coefficient for solar radiation drop a little bit from -1, but we can draw a horizontal line in the grey area (Confidence Interval),
#so when wind changes,the effect of solar radiation doesn't change a lot, so these two factors are not interactive.
interplot(mod3,var1="Solar.R",var2="Wind")+
  labs(x="Solar radiation",y="coefficient for wind")
#When solar radiation become higher, the coefficient for wind drop a little bit，but we can draw a horizontal line in the grey area (Confidence Interval),
#this means when solar radiation changes,the effect of wind to temperature doesn't change a lot, so these two factors are not interactive.
