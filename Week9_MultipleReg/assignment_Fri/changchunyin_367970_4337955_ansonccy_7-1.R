data("airquality")
head(airquality, 15)

library(dplyr)
airquality1 = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)


# exercise 1: Manipulate the above code to reduce the correlation between X1 and X2 and recalculate the regression coefficients. 
# Explain what you find. 

Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=20)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=20)
df = as.data.frame(cbind(Y, X1, X2))

plot(X1~X2)
cor(X1,X2)

summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))

# The noise range for both X1 and X2 have been increaed from 0.5-2 to 0.5-20. As the random noise is increased, X1 and X2 are less correlated. 
# Thus, from the plot, there is no any correlation pattern observed. 
# The correlation coefficient is 0.234 which means the interdependency is pretty low.
# The recalculated regression coefficient for X1 is around 0.34 and X2 is around 0.46.


# Exercise 2: Compare a third model (mod3) with ozone varaible as the third independent variable to mod1 and mod2 above. 
# Does it explain more variance? Does it give you reasonable estimate of regressino coefficients?
mod1 = lm(Temp~Wind, data=airquality1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality1)
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality1)
anova(mod1,mod2)
anova(mod1, mod2, mod3)
# by adding ozone as the third independent variable in mod3, mod3 explains significantly more variance 
# since the probability for mod1 to explain the same amount of variance as mod3 is 2.424e-09

# Exercise 2 bonus: Challenge yourself by calculateing theRSSRSSof the two models,FFstatistics (1 pt) and the p-value (1 pt) all by yourself 
res1=residuals(mod1)
res2=residuals(mod2)
RSS1=sum(res1^2)
RSS2=sum(res2^2)
str(summary(mod1))
str(summary(mod2))

F=((RSS1-RSS2)/(summary(mod1)$df[2]-summary(mod2)$df[2]))/(RSS2/summary(mod2)$df[2])
# manually calculated F value = 8.374
anova(mod1,mod2)
# from the anova, F value = 8.374 which matches with my manual calculation

# Exercise 3: Compare the AIC of the three models above mod1, mod2, and mod3. Do you have different conclusion in terms of which model performs better?
AIC(mod1,mod2,mod3)

# From the AIC result, mod3 is the best model because the AIC result is the lowest, 748.
# The result is consistent with the method of using anova for the comparison.
