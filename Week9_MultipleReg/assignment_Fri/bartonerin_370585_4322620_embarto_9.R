#Erin Barton
#NRE538
#3.13.2017

#Lab Write-Up 9

#Exercise 1: Manipulate the above code to reduce the correlation between X1 and X2 
#and recalculate the regression coefficients. Explain what you find.
Y1 = seq(from=0, to=20, by=0.1)
Xa = (Y1-2)/3 + runif(length(Y1), min=0.5, max=7)
Xb = (Y1+3)/2 + runif(length(Y1), min=0.5, max=7)
df = as.data.frame(cbind(Y1, Xa, Xb))
summary(lm(Y1~Xa, data=df))
summary(lm(Y1~Xb, data=df))
summary(lm(Y1~Xa+Xb, data=df))
plot(Xa~Xb)
cor(Xa,Xb)
#Increasing the denominator decreases the amount of correlation between the two 
#variables, but manipulating the numerator does not. This is likely because the denominator 
#is the slope, or regression coefficient. Increasing the amount of noise also decreases
#the amount of correlation. This is because the noise is random and more of it creates
#more differentiation between the two models. 

#Exercise 2: Compare a third model with ozone variable with third independent variable to mod1
#and mod2 using anova. Does it explain more variance? Does it give you a reasonable estimate of 
#regression coefficients? 
mod1=lm(Temp~Wind, data=airquality1)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality1)

summary(mod2)
mod3=lm(Temp~Wind+Solar.R+Ozone, data=airquality1)
summary(mod3)

anova(mod1,mod2, mod3)
#This comparison shows that mod3 explains a greater amount of variance than mod1 or mod2. 
#We can tell because the p-value is very low, which indicates that the probability of observing 
#the model differences given random processes is very low. This means that mod3 explains more variance
#than mod2. 

#Bonus: Calculate RSS of two models, F stat, and pval without using anova()
n=length(airquality1)

res1 <- residuals(mod1)
RSS1 <- sum(res1^2)
res2 <- residuals(mod2)
RSS2 <- sum(res2^2)
Fstat1 <- ((RSS1-RSS2)/1)/(RSS2/n-3)
Fstat1

#Exercise 3: Compare the AIC of the three models above, mod1, mod2, mod3. Do you have different
#conclusions in terms of which model performs better? 
AIC(mod1, mod2, mod3)
#According to the AIC, mod3 performs better. This is the same result that came out of the anova comparison. 
