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
## The first plot shows that the effect of root depth on fruit weight is higher in ungrazed areas than grazed areas, but the difference is not significant.
## The second plot shows that the effect of grazing on fruit weight increases as root depth increases, but the interaction is not significant.

#Exercise 2
data("airquality")
mod3 = lm(Temp~Wind + Ozone, data = airquality)
summary(mod3)
mod4 = lm(Temp~Wind*Ozone, data = airquality)
summary(mod4)

anova(mod3, mod4)
##The f-test shows that when we added an additional degree of freedom for mod4, the residual sum of squares dropped significantly.
##This means that the models are significantly different, and mod4 has a lower RSS, so is a better fit.
##The adjusted R squared value from the summary outputs confirm that mod4 explains more variation in the data.
##Therefore, mod4 is a better model.

mod5 = lm(Temp~Wind*Solar.R, data = airquality)
summary(mod5)

interplot(mod5, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar.R")
interplot(mod5, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="coefficient for Wind")
##As wind increases, the effect of Solar.R decreases slightly.
##As wind decreases, the effect of Solar.R increases slightly.
##As Solar.R increases, the effect of wind decreases slightly.
##As Solar. R decreases, the effect of wind increases slightly.
##However, the changes in one of the variables when the other changes are not significant (from the p-value in the summary output).