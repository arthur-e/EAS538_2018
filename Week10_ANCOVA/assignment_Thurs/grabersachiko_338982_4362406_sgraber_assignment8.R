#####EXERCISE 1
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"),
                sep = "", header=T, comment.char="#")
head(gz)

shapiro.test(gz[,"Root"]) #p=0.6104 so we cannot reject the null hypothesis that data are normal. Assume data are normal.
shapiro.test(gz[,"Fruit"]) #p=0.7798 so we cannot reject the null hypotehsis that data are normal. Assume data are normal.

modgz=lm(Fruit~Root*Grazing, data=gz)
summary(modgz)
#Plants with grazed treatment with 0 root length have weight of -125.
#Each additional unit of root depth adds 23 units of weight to a fruit.
#Plants with treatment ungrazed have 31 more weight units than plants with treatment grazed
#The difference in effect of root length on ungrazed treatment fruit is 0.8 weight units more than the effect of root units on grazed treatment
anova(modgz)
interplot(m=modgz, var1="Root",var2="Grazing") + 
  labs(x="Grazing treatment", y="Estimated coefficient for Root length",
       title = "Estimated Coefficient of Root length on Grazing treatment")
#The effect of root length on fruit weight is higher for ungrazed than grazed treatment, but not significantly so. 
interplot(m=modgz, var1 = "Grazing", var2 = "Root") + 
  labs(x="Root length", y="Estimated coefficient for grazing treatment",
       title = "Estimated coefficient of grazing treatment on root length")
#Changing grazing treatment to ungrazed has a positive effect on fruit weight, and the difference becomes greater as root length increases.
#This change of weight difference is not significant.


#####EXERCISE 2
###1. 
mod1 = lm(Temp~Wind + Ozone, data=airquality)
mod2 = lm(Temp~Wind*Ozone, data=airquality)
anova(mod1, mod2) #mod1 and mod2 are significantly different, as we see from the very small p-value.
#mod2 explains significantly more of the variance in fruit weight as compared to mod1. 
#We know this from the much smaller RSS value, indicating that there is much less error.
#Therefore, I choose mod2 to explain data if I must choose one model.

###2. Plot two interaction plots with interplot for the model with wind and solar radiation as independent variables. interpret.
mod3 = lm(Temp~Wind*Solar.R, data=airquality)
interplot(m=mod3, var1="Wind", var2="Solar.R") + labs(x="Solar Radiation", y="Estimated coefficient for Wind",
                                                      title="Estimated coefficient of wind on solar radiation")
#With increased solar radiation, the negative effect of [a one-unit increase in wind] on temperature becomes MORE negative.
#However, the change in temperature is not significant.

interplot(m=mod3, var1="Solar.R", var2="Wind") + labs(x="Wind", y="Estimated coefficient for Solar Radiation",
                                                      title="Estimated Coefficient of Solar Radiation on Wind")
#With increased wind, the positive effect of [a one unit change in solar radiation] on temperature decreases. 
#However, this change is not significant.