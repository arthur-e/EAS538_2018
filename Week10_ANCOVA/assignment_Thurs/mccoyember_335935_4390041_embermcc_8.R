install.packages("Lahman")
install.packages("ggplot2")

library(Lahman)
library(dplyr)
library(ggplot2)
library(RCurl)
library(interplot)

## EXERCISE 1 

gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)

shapiro.test(gz[,"Root"]) #p-value = 0.6104 so normal
shapiro.test(gz[,"Fruit"]) #p-value = 0.7798 so normal

mod = lm(Fruit~Root*Grazing,data=gz)
aov = aov(Fruit~Root*Grazing,data=gz)
summary(mod) #interaction term p value = 0.7500 (not significant)
anova(mod)
summary(aov) #interaction term p value = 0.75 (not significant)

interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root Depth", title="Estimated coefficient of Rooth Depth on Grazing")
#This plot means that the effect (regression coefficient) of Root Depth on Fruit weight is higher in ungrazed areas over grazing areas, 
# but the difference is not significant at the .05 level. 

interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root Depth", y="Estimated coefficient for Grazing", title="Estimated coefficient of Grazing on Root Depth")
#This plot means that fruit weight for ungrazing areas is higher than grazing areas, and the difference gradually increases as root depth increases. However, this change in fruit weight is not significant at the .05 level. 

## EXERCISE 2 

data("airquality")
head(airquality, 15)

# part 1 
mod1 = lm(Temp~Wind+Ozone, data=airquality)
summary(mod1) 
plot(Temp~Wind+Ozone, data=airquality)
abline(lm(Temp~Wind+Ozone, data=airquality, col="red"))

mod2 = lm(Temp~Wind*Ozone, data=airquality)
summary(mod2) 

anova(mod1,mod2)
#f statistic p value = 6.927e-05 ***, therefore mod1 and mod2 are significantly different. 
#mod1 RSS = 5166.5, mod2 RSS=4482.7
##Because the RSS value for mod2 is less than mod1 (has less error), we can say that mod2 is better fit for the data than mod1 (it explains more of the variance in fruit weight compared to mod1), so I would choose mod2. 

# part 2 

mod4 = lm(Temp~Wind*Solar.R, data=airquality)
summary(mod4) # interaction term p value = .7537

interplot(mod4, var1="Wind", var2="Solar.R")+ labs(x="Wind", y="coefficient for Solar Radiation")
#This plot means that the effect (regression Coefficient) of solar radiation on temperature increases as wind decreases, but the effect is not significant at the .05 level.  

interplot(mod4, var1="Solar.R", var2="Wind")+labs(x="Solar Radiation", y="Coefficient for Wind")
#This plot means that the effect (regressioncoefficient) of wind on temperature increases as solar radiation decreases, but the effect is not siginficant at the .05 level.
