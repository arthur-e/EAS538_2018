###Exercise 1
install.packages("RCurl")
library("RCurl")
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
mod = lm(Fruit~ Root*Grazing, data=gz)
summary(mod)
library("interplot")

interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Grazing")
###This plot means the effect (regression coefficient) of Root on fruit weight 
###is higher in Ungrazed than in the Grazed, but the difference is NOT significant.

interplot(mod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing",title="Estimated Coefficient of Grazing on Root")
###This means the diffrences between grazed and ungrazed fruit weight is keep increasing when the root depth is increasing. 
###Which is caused by the effect of root which is higher in ungrazed treatment(toward right end) than grazed.

##### Oscar: No, the interaction term is not significant, 
#####        so you can NOT say there is any effects of one varaible on the coefficient of the other variable. 



###Exercise2
head(airquality)
mod1 = lm(Temp~Wind + Ozone, data=airquality)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
anova(mod1, mod2)
###   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
###1    113  5166.5                                  
###2    112   4482.7  1    683.81 17.085 6.927e-05 ***
######The interpretation of these results is that mod2 explains significantly greater amount of varaince comparing to mod1 (mod1 has smaller RSS).


mod3= lm(Temp~Wind * Solar.R, data=airquality)
summary(mod3)
interplot(mod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar.R")
### This means the temperature in low solar radiation condition is first slightly 
###lower than in high sloar radiation condition (negative intecept of this figure), 
###but the difference gradually increase with the increase of wind speed.When the wind 
###is stronger, the temperature in low solar radiation condition will be much more lower 
###than the high solar condition compare with the situation which not influenced by wind factor.

##### Oscar: No, the interaction term is not significant, 
#####        so you can NOT say there is any effects of one varaible on the coefficient of the other variable. 

interplot(mod3, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="coefficient for Wind")
###This means the temperature in low wind speed is first slightly higher 
###than high wind speed condition. But this difference gradully derease 
###with the increse of solar radiation.






