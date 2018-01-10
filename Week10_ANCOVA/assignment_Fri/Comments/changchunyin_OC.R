# Exercise 1
# Try to use the following data set to execute ANCOVA and make interaction plots 
# This data set contains the fruit weight (Fruit) of different plants with different root depth (root) and different treatment (Grazing).

library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)

mod=lm(Fruit~Root*Grazing, data=gz)
summary(mod)

library(interplot)
interplot(m=mod,var1 = "Root", var2 = "Grazing")+labs(x="root depth",y="estimated coefficient of grazing", title="Estimated coefficient of grazing on root depth")

interplot(m=mod,var1 = "Grazing", var2 = "Root")+labs(x="treatment", y="estimated coefficient of root depth", title="Estimated coefficient of root depth on treatment")

# From the result, Fruit weight = -125.173 + 23.240(Root deepth) + 30.806(treatment) + 0.756(root deepth)(treatment)
# Which means the intercept is -125.173, when root deepth and treatment are both zero 
# For grazing treatment, a unit of increase in root deepth will result in 23.240 units of increase in fruit weight
# For ungrazed treatment, a unit of increase in root deepth will result in 23.240+0.756= 23.996 units of increase in fruit weight 
# From the first interplot, the difference between the dots is 0.756 which represents under different treatment, the difference on effect of root depth to fruit weight
# From the second interplot, the slope of the line is 0.756 which represents under different treatment, the difference on effect of root depth to fruit weight
# However, the p-value for both treatment and interaction are not significant which means their explainary effect are dominated by Root deepth, so the effect is negligible

# Exercise 2
# Use F-test to compare mod.1 and mod.2. Explain why you chose one over another to explain the data.
mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)

anova(mod1,mod2)
# From the variance table, since the probability that mod2 can explain the same variance with mod1 is very small (p-value is significant),
# so mod2 is explaining different variance
# Also, mod2 has a lower RSS, which means it has more predictable sum of square than mod1

# Plot two interaction plots with interplot() for the model with wind and solar radiation as the independent variables. Interprete the two interaction plots.
head(airquality)
mod3 = lm(Temp~Wind*Solar.R, data=airquality)
summary(mod3)

interplot(m=mod3,var1 = "Wind",var2="Solar.R")+labs(x="Wind",y="estimated cofficint of Solar.R", title="Estimated coefficient of Solar.R on Wind")
# From the first interplot, the slope of the line is -0.00065 which represents that as wind increase,
# the effect of solar.R on temperature decrease with a slope of -0.00065. 
# slope of solar.R = 0.0328-0.00065*wind
# The interaction term here is not significant since the p value is 0.7537 (>0.05),
# And also from the graph, we can draw a horizontal line within the grey area that represents the confidence interval

##### Oscar: Since the interaction term is not significant, the slope of solar.R is 0.0328, NOT 0.0328-0.00065*wind!

interplot(m=mod3,var1 = "Solar.R",var2="Wind")+labs(x="Solar.R",y="estimated cofficint of Wind", title="Estimated coefficient of Wind on Solar.R")
# From the second interplot, the slope of the line is -0.00065 which represents that as solar.R increase,
# the effect of wind on temperature decrease with a slope of 0.00065. 
# slope of wind = -1.04-0.00065*solar.R
# The interaction term here is not significant since the p value is 0.7537 (>0.05),
# And also from the graph, we can draw a horizontal line within the grey area that represents the confidence interval