#Exercise 1
#Manipulate the above code to reduce the correlation 
#between X1X1 and X2X2 and recalculate the regression 
#coefficients. Explain what you find.

#increase random noise- large noise will lower the correlation coef
# play around with code, as correlation declines the corcof will get
#closer to original

#should see a more precise estimate as they become less correlated

data("airquality")
data("airquality")
library(dplyr)
head(airquality, 15)
airquality = airquality %>% 
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))


cor(X1,X2,use="na.or.complete") 
#correlation originally betweeen x1 and x2 was 0.9, after adjustments
#with min = 0.1 and max=50, then the correlation changed to -0.02 which 
#signifies that the two variables are near random.

summary(lm(Y~X1, data=df))  
plot(X1~X2)

#Coefficients for X1 = 0.03540 and an intercept of 9.03 X1 now is
#signifies that there is no longer a correlation and that the noise
#obscures the values of X1 And

#Exercise 2

#Bonus -Challenge yourself by calculateing the 
#RSSof the two models, 
#F statistics (1 pt) and the p-value (1 pt) all by yourself (i.e. not using the anova() function)

res = residuals(mod1)
RSS = sum(res^2)
summary(RSS)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   7521    7521    7521    7521    7521    7521 

MSE = RSS/summary(mod1)$df[2]
SSY = deviance(mod1)
R2 = 1 - RSS/SSY
F = ((SSY-RSS)/1)/MSE
F = # 8.451469


res = residuals(mod2)
RSS = sum(res^2)
summary(RSS)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#6980    6980    6980    6980    6980    6980 

MSE = RSS/summary(mod2)$df[2]
SSY = deviance(mod2)
R2 = 1 - RSS/SSY
F1 = ((SSY-RSS)/1)/MSE
F1 = #0



#Compare a third model (mod3) with ozone varaible 
#as the third independent variable to mod1 and mod2 above. 
#Does it explain more variance? Does it give you reasonable estimate of regressino coefficients?

mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)

mod3= lm(Temp~Wind+Solar.R+Ozone, data=airquality)

anova(mod1,mod2,mod3)
# RSS for mod 1= 7520, mod2=6979, and mod3=4996, with an F statistic increasing from 2 to 3 (11 to 42).
#This indicates model 3 explains greater variance. 
#In summary the reduction of  RSS for model 3 demonstrates a significant drop to the others 
#and the P value is signifcant (p=2.424e-09) demonstrating that it explains a greater variance



#Exercise 3
#Compare the AIC of the three models above mod1, mod2, and mod3. 
#Do you have different conclusion in terms of which model performs better?

AIC(mod1) - #788.9671
AIC(mod2) - #782.6779
AIC(mod3) - #747.5795
  
#AIC for model 3 is lower indicating that model three did explain more variance than
  #the other two
