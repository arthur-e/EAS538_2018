install.packages("Lahman")

library(Lahman)
library(dplyr)
library(ggplot2)

#Exercise
install.packages("RCurl")
install.packages("bitops")
library(bitops)
library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")

shapiro.test(gz$Root)
shapiro.test(gz$Fruit)

modf = lm(Fruit~ Root*Grazing, data=gz)
aovf = aov(Fruit~ Root*Grazing, data=gz)

summary(modf)
anova(modf)
summary(aovf)
#From the output we see that the interaction term is not significant.This means that the effect of one variable does not depends on the other one. In the case, we can say the relationship between Fruit and Root is not significantly different from grazed to ungrazed.


interplot(m=modf, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Grazing")
#the effect of Root on fruit is not that higher in Ungrazed than in the Grazed.

interplot(m=modf, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Root on Grazing")

#The Fruit Weight is higher in ungrazed compared to grazed but as root value increases this difference increases in very smal quantities. Thus the change of fruit weight difference is not significant.

#Exercise 2
data("airquality")
head(airquality, 15)

mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)

anova(mod1,mod2)
#The model 2 exhibits more variance compared to model1 and model 2 as explained by its regression coefficient which is 4482.7 (the other 5166.5)
#The F-statistic is 17.085 and p value being extremely low which indicates that the probablity with which model 1 can explain same amount of varience in model 2 is nearly 0.

mod = lm(Temp~Wind * Solar.R, data=airquality)
summary(mod)
#There is no significant interaction between Wind and Solar.R

interplot(mod, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar,R")
#Coefficient of Solar.R decreases slightly with increase in Wind. But this is not signicant as the line is close to being horizontal.

interplot(mod, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="coefficient for Wind")
#Coefficient of Wind decreases slightly with increase in Solar.R But this is not signicant as the line is close to being horizontal.