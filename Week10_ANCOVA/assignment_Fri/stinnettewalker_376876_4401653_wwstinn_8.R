### Exercise 1 ###

library(RCurl)
library(lmtest)

gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)

## ANCOVA ##
mod = lm(Fruit~ Root*Grazing, data=gz)

shapiro.test(residuals(mod))
# Fail to reject null hypothesis that residuals follow a normal distribution (p-value = 0.559)
# Residuals are normally distributed
bptest(mod)
# Fail to reject null hypothesis that residuals are homoscedastic (p-value = 0.6456)
# Residuals are homoscedastic
dwtest(mod)
# Fail to reject null hypothesis that residuals are independent (p-value = 0.4879)
# Residuals are independent

summary(mod)
# The intercept (-125.173) is fruit weight in grazed plots when root depth is 0.
# In grazed plots, one unit increase in root depth results in a significant (p-value = 2e-16) 23.240 unit increase in fruit weight
# After controlling for root depth, there is not a significant (p-value = 0.0757) difference in fruit weight between grazed and ungrazed plots
# The interaction term is not significant (p-value = 0.75). Thus, the relationship between root depth and fruit weight is not significantly different between grazed and ungrazed plots
# The multiple R-squared value indicates that the model explains 92.93% of the variation in fruit weight.
# The p-value = 2.2e-16, which suggests that there is a very small likelihood that the model explains variation in fruit weight by chance.

## Interaction Plots ##
library(interplot)

interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Plot treatment", y="Estimated coefficient for root depth", title="Estimated Coefficient of Root Depth on Plot Treatment")
# The regression coefficient of root depth on fruit weight is higher in ungrazed than in the grazed, but the difference is NOT significant.

interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root depth", y="Estimated coefficient for plot treatment", title="Estimated Coefficient of Plot Treatment on Root Depth")
# Fruit weight in ungrazed plots is higher than in grazed plots (intercept at 30.806), and this difference gradually increases with the increase root depth. However, this difference in fruit weight is NOT significant.

### Exercise 2 ###

data("airquality")
mod1 = lm(Temp~Wind + Ozone, data=airquality)
mod2 = lm(Temp~Wind * Ozone, data=airquality)

## Comparing linear models with F-test ##
anova(mod1, mod2)
# The RSS value for Model 1 is 5166.5 and for Model 2 is 4482.7.
# Therefore, Model 2 explains the variation in temperature significanlty (p-value = 6.927e-05) better than Model 1
# Based on the significant difference in the RSS values, I would use Model 2 because it is a better fit for the data

## Interaction Plots ##
mod3 = lm(Temp~Wind * Solar.R, data=airquality)
summary(mod3)
# From the summary table for Model 3, there is no significant (p-value = 0.7537) interaction between wind and solar radiation
interplot(mod3, var1="Wind", var2="Solar.R")+
  labs(x="Solar Radiation", y="Coefficient for Solar Wind", title="Estimated Coefficient of Solar Radiation on Wind")
# As solar radiation increases, the effect of wind on temperature decreases. But this interaction is not significant.
interplot(mod3, var1="Solar.R", var2="Wind")+
  labs(x="Wind", y="coefficient for Solar Radiation", title="Estimated Coefficient of Wind on Solar Radiation")
# As wind increases, the effect of solar radiation on temperature decreases. But this interaction is not significant. 
# The nearly horizontal lines in both interplots reinforce the fact that the interaction between wind and solar radiation is not significant.


