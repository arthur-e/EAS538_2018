###Exercise 1
library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
str(gz) #two numerical factors and one categorical
library(ggplot2)
ggplot(data=gz, mapping=aes(x=Root, y=Fruit, color=factor(Grazing)))+
  geom_point() #slopes look similar despite different intercepts
ggplot(data=gz, mapping=aes(x=Root)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
shapiro.test(gz[,"Root"]) #root variable is normal
shapiro.test(gz[,"Fruit"]) #fruit variable is normal
fruit.mod = lm(Fruit~Root*Grazing, data=gz)
summary(fruit.mod)
library(interplot)
interplot(m=fruit.mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root depth", title="Estimated Coefficient of Root depth on Grazing")
interplot(m=fruit.mod, var1="Grazing", var2="Root")+
  labs(x="Root depth", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Root depth")
#Results:
#There are no significant differences between the effect of root depth on fruit weight in ungrazed vs. grazed treatments
#There are significant main effects of root depth on fruit weight.

###Exercise 2
data("airquality")
mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)
anova(mod1, mod2)
#These models are nested, so we can use an F test.
#mod2 explains significantly more variance in the model than mod1 due to the lower sum of squares value of mod2, and the probability that mod1 can explain the same variance is approximately 0 (6.9e-05). The F statistic is also telling of the significant difference with a value of 17.085.

###Exercise 3
mod3 = lm(Temp~Solar.R * Wind, data=airquality)
summary(mod3)
interplot(m=mod3, var1="Wind", var2="Solar.R")+
  labs(x="Solar.R", y="Estimated coefficient for Wind", title="Estimated Coefficient of Wind on Solar.R")
interplot(m=mod3, var1="Solar.R", var2="Wind")+
  labs(x="Wind", y="Estimated coefficient for Solar.R", title="Estimated Coefficient of Solar.R on Wind")
#Results:
#The effect of wind on temperature decreases as solar radiation levels increase but the difference is not significant
#The effect of solar radiation on temperature decreases as wind levels increase but the difference is not significant
?interplot
