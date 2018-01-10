data("airquality")
head(airquality, 15)

install.packages("dplyr")
library(dplyr)

airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

#####EXERCISE 1

#original
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df)) #coefficient = 2.81401 , intercept =  -1.02384 
summary(lm(Y~X2)) #coefficient = 1.9902, intercept =  -5.4822  
summary(lm(Y~X1+X2, data=df))
plot(X1~X2)
cor(X1, X2) #0.9643281

#attempt to change 
Y.new = seq(from=0, to=20, by=0.1)
X1.new = (Y.new-2)/3 + runif(length(Y.new), min=0.5, max=19) #increased max from 2 to 20
X2.new = (Y.new+3)/2 + runif(length(Y.new), min=0.5, max=19) #increased max from 2 to 20
df.new = as.data.frame(cbind(Y.new, X1.new, X2.new))
summary(lm(Y.new~X1.new)) #coefficent =  0.30159, intercept =  6.32243
summary(lm(Y.new~X2.new)) #coefficient=0.43606, intercept = 2.93060
summary(lm(Y.new~X1.new+X2.new, data=df.new))  
plot(X1.new~X2.new)
cor(X1.new, X2.new) #0.189586 
##Increasing the max from 2 to 20 to create more noise decreased the correlation from 0.9643281 to 0.189586
##BUT this method makes it so that the coefficient estimate for x1 and x2 are more different from the true values (this is a tradeoff)

#### BONUS

#given from exercise
mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)
anova(mod1, mod2) #fstat = 8.3739, p value=0.004606


#manual calculation of RSS & degrees of freedom
res.mod1 = residuals(mod1)
RSS.mod1 = sum(res.mod1^2)

res.mod2 = residuals(mod2)
RSS.mod2 = sum(res.mod2^2)

df1 = 3-2
df2 = (length(airquality$Temp)) - 3

#manual calculation of f statistic
numerator = (RSS.mod1 - RSS.mod2)/df1
denomenator = RSS.mod2/ df2

f.stat = numerator/denomenator 
f.stat #8.373933 matches f statistic from anova(mod1, mod2) above! 
pvalue = 1 - pf(f.stat,df1,df2)
pvalue # 0.004605629 matches p value from anova(mod1, mod2) above!

#### EXERCISE 2 

mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod1, mod2, mod3) 
# RSS comparing mod2 to mod1 is 6979.5, f statistic is 11.589, p value = 0.0009349 (significant)
# RSS comparing mod3 to mod2 is 4996.6, f statistic is 42.463, p value = 2.424e-09 (significant)
## Because for model 3 the RSS decreased, and the f-stastic increased, it explains more variance 
## The regression coeffients did change when adding ozone (in model 3) -  solar radiation & ozone are no longer significant at the .05 level, but the signs and sizes are still reasonable  

#### EXERCISE 3 

AIC(mod1) #788.9671
AIC(mod2) #782.6779
AIC(mod3) #747.5795

##Because model 3 has the lowest AIC value, you can conclude that model 3 performs better  

