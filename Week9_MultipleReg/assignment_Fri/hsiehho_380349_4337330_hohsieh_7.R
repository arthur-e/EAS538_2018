#Ho Hsieh_NRE538_lab7

#Exercise 1####
Y.ex = seq(from=0, to=20, by=0.1) 
X1.ex = (Y-2)/3 + runif(length(Y.ex), min=100, max=500) #increase the random noise
X2.ex = (Y+3)/2 + runif(length(Y.ex), min=0.5, max=2)
df.ex = as.data.frame(cbind(Y.ex, X1.ex, X2.ex))
summary(lm(Y.ex~X1.ex, data=df)) #the X1 coefficient = 0.0002709 

summary(lm(Y.ex~X2.ex, data=df)) #the X2.ex coefficient = 1.9929 

summary(lm(Y.ex~X1.ex+X2.ex, data=df)) #X1 coe=-0.0001647; X2 coe =1.9929379

plot(X1.ex~X2.ex) 
cor(X1.ex, X2.ex) #0.008720205

#When I increase the random noise of X1.ex, the coefficient of X2 
#does not change from the true value when I put both X1.ex and X2.ex into the linear model
#while the coefficient of X1.ex still change a little from the true value
#The correlation coefficient of X1.ex and X2.ex become 0.008720205 when I increase the random noise of X1.ex


#Exercise 2####
#2-Bonus####
res_1 = residuals(mod1)
RSS_1 = sum(res_1^2)

res_2 = residuals(mod2)
RSS_2 = sum(res_2^2)
n = length(res_1)
p1 = 2
p2 = 3

Fstats = ((RSS_1-RSS_2)/(p2-p1))/(RSS_2/(n-p2))
Fstats #8.373933
pvalue = pf(8.373933, 1, 108, lower.tail = F)
pvalue #0.004605628
#F statistics = 8.373933; p-value = 0.004605628


#2-1####
data("airquality")
airquality1 = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE) #remove the NA in the data

mod1 = lm(Temp~Wind, data=airquality1) #Estimate: -1.3318(Wind) 
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality1) #Estimate: -1.251870(Wind); 0.024533(Solar.R)
summary(mod2)
mod3 = lm(Temp~Wind+Solar.R+Ozone, data = airquality1)
summary(mod3) #Estimate: -0.322945 (Wind);  0.007276 (Solar.R);  0.171966 (Ozone)

anova(mod1, mod2) #p-value=0.004606
anova(mod2, mod3) #p-value=2.424e-09 << 0.05

#mod3 explains significantly greater amount of varaince comparing to mod2, 
#because the probability for mod2 to explain the same amount of variance 
#as mod3 is 2.424e-09.

#Since the result suggests that mod3 is better than mod2
#in explaining the variance,
#the estimate of regression coefficients should be more reasonable

mod4 = lm(Temp~Ozone, data = airquality1)
summary(mod4) #Estimate: 0.20006 (Ozone) 
mod5 = lm(Temp~Ozone+Solar.R, data = airquality1)
mod6 = lm(Temp~Ozone+Wind, data = airquality1)
anova(mod4, mod5) #p-value = 0.432
anova(mod4, mod6) #p-value = 0.202
#From comparing mod4 to mod5 and mod6, 
#we can know that Ozone explain most amount of the variance in the response variable



#Exercise 3####

AIC(mod1) #788.9671
AIC(mod2) #782.6779
AIC(mod3) #747.5795

AIC(mod1)-AIC(mod2) #6.289184
AIC(mod1)-AIC(mod3) #41.38763
AIC(mod2)-AIC(mod3) #35.09844

#Given the results of AIC, 
#mod1 and mod2 are not significantly different if using 10 as the standard for significant difference
#while mod3 is significantly different from mod1 and mod2

#The result of AIC is different from that of F test, 
#in which mod1 is also significantly different from mod2
