#EX1
install.packages("RCurl")
library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
mod = lm(Fruit~ Root*Grazing, data=gz)
summary(mod)
#The intercept is fruit weight of Grazed plant (alphabetical order) when root depth is zero. 
#The estimate for GrazingUngrazed is the average difference in fruit weight between the two treatment groups.
#The estimate for Root depth is the effect of root depth on Grazed plants. 
#Interaction term is NOT sifnificant; the relationship between fruit weight and root depth is not significantly different from Grazed to Ungrazed treatments.
library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root Depth", title="Estimated Coefficient of Root Depth on Treatment")
#The effect of Root Depth on Fruit Weight is higher in Ungrazed, but the difference is not signficiant between treatments.
interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Root Depth")
#Fruit weight of Grazed treatments is lower than Ugrazed, and the difference between Fruit Weight of Grazed and Ungrazed treatments gradually increases with root depth.But the effect of Treatment on Fruit Weight is NOT significant.

#EX2
mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)
anova(mod1, mod2)
#I would use Model 2 because it minimizes Residual Sum of Squares compared to Model 1, therefore Model 2 explains more variance in the data than Model 1, and the probability that Model 1 could explain the same amount of variance as Model 2 is 6.9E-05, which is very small.
library(interplot)

mod3= lm(Temp~Wind*Solar.R, data=airquality)
summary(mod3)
interplot(mod3, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="coefficient for Wind")
interplot(mod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar.R")
#Temperature increases more due to Wind than Solar.R, and this difference in temperature continues to increase with more Wind;however the interaction between Solar.R and Wind is not significant.