#Exercise 9 

library(dplyr)
data("airquality")
head(airquality, 15)
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
pairs(airquality[,c(1:4)])
cor(airquality[,c(1:4)])

#Exercise 1
y = seq(from=0, to=20, by=0.1)
x1 = (y-2)/3 + runif(length(y), min=0.5, max=15)
x2 = (y+3)/2 + runif(length(y), min=0.03, max=5)
df = as.data.frame(cbind(y, x1, x2))
summary(lm(y~x1, data = df))
summary(lm(y~x2, data = df))
summary(lm(y~x1+x2, data = df))
plot(x1~x2)
#As can be shown in the summary code above, as well with the plot(x1~x2), we see that x1 and x2 are no longer correlated. This is because we have altered the random noise. Before this exercise, the slopes as well as the random noise were highly similar, and because of this, x1 and x2 were highly correlated. When we change the random noise, we change their correlation with one another. 

#F-Test
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)

anova(mod1, mod2)

#Exercise 2
mod3=lm(Temp~Wind+Solar.R+Ozone, data = airquality)
summary(mod3)

anova(mod1, mod2, mod3)
#From these results, we see that adding on a thrid variable (ozone) to our model, explains more variance. As shown by the results from model 3, we have a larger F value, as well as a larger P value, which shows that more varriance is explained when adding a third variable. This means that model 3 explains signficantly greater variance than the other models (mod1, mod2), due to the higher pvalue. Therefore, having more variables can explain more variance, especially when adding in a factor like Ozone. This is due to the fact that these variables are nested, which is needed to run an F-test.

#Exercise 3
AIC(mod1, mod2, mod3)
#After running the AIC test, we see that model three has a lower AIC value. Therefore, model 3 would have a larger probability of explaining what is going on. The lower AIC value of model 3 shows that model 3 is the best model. This is because it loses the least amount of information when the most parameters are added. Model three is different from models one and two by 10, therefore, it is more signifciant and the best model. 
