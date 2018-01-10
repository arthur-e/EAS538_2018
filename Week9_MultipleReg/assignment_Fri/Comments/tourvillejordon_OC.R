###Data load, clean, basic interpretation
data("airquality")
head(airquality, 15)
#install.packages("dplyr")
library(dplyr)
airquality1 = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
pairs(airquality1[,c(1:4)])
cor(airquality[,c(1:4)], use="na.or.complete")
###Exercise 1: Adjust code to reduce cor between X1 and X2 and report what effect 
#this has on regression coefficient values.
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=20)#changed from max of 2 to 20
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=20)#changed from max of 2 to 20
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))
plot(X1~X2)
cor(X1, X2)#as the amount of error (noise) increases, the correlation between x1 and x2 decreases.
#However, the regression coefficients are still very off from where they should be (around 0.3 for both instead of 3 for x1 and 2 for x2).

##### Oscar: Any idea why is that?

###
###Exercise 2
mod1 = lm(Temp~Wind, data=airquality1)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality1)
summary(mod2)
mod3 = lm(Temp~Wind+Solar.R+Ozone, data = airquality1)
summary(mod3)#Model 3 explains more of the variance in the dependent variable (Adjusted R2 = 0.4858) than the other two models.
anova(mod1, mod2, mod3)#Model 3 explains more variance than the other models and has reasonable coefficient estimates based on anova table (Smaller RSS and significant).

##### Oscar: Not necessary. Explaining more variance does not mean there is no correlation among independent variables.
#####        Whether a regression coefficient is reasonable depends on whether this varaible is correlated with others.
##### Oscar: Model comparison does NOT tell us which "variable" explain more variation. 
#####        It only tell us which "model" explain more variance. 
#####        You also can NOT use f-test to compare different univariate models (not nested apparently). 
#####        There is no way you can judge which variable explains more without doing variance partitioning.

###
###Exercise 3
AIC(mod1, mod2, mod3)#model 3 has the lowest AIC value (747.5795, greater than 10 difference) so we would conclude that this is the best model to use of the three.