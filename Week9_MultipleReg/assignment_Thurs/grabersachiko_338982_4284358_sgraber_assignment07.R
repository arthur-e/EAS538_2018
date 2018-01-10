#####EXERCISE 1
Y = seq(from=0,to=20,by=0.1) #generates 201 Y values from 0-20 (known Y vector)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=10) #Y = 3X1 + 2 + *increased* noise
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=10) #Y = 2X2 - 3 + *increased* noise
df = as.data.frame(cbind(Y,X1,X2))
cor(X1, X2) #the correlation coefficient decreases significantly
plot(X1~X2) #we see visually that there is less correlation
summary(lm(Y~X1, data=df)) #B1 = 1.0
summary(lm(Y~X2, data=df)) #B2 = 1.0
summary(lm(Y~X1+X2, data=df)) #B1 = 0.7, B2 = 0.8
#we see that there is much less change in the regression coefficients calculated by univariate vs. 
#multivariate models, indicating that there is less collinearity between our explanitory variables


#####EXERCISE 2
#Bonus
res1 = residuals(mod1)
RSS1 = sum(res1^2) #RSS = 7520.7 - same as using anova
res2 = residuals(mod2)
RSS2 = sum(res2^2) #RSS = 6979.5 - same as using anova

length(airquality$Temp)
Fstat = ((RSS1-RSS2)/(3-2))/(RSS2/(length(airquality$Temp)-3)) #F-stat = 8.3739 as we saw in the anova

pvalue = pf(Fstat,1,108,lower.tail=F)
pvalue #0.00406 as we saw in the anova

#1. 
mod3 = lm(Temp ~ Wind + Solar.R + Ozone, data=airquality)
summary(mod3)
#The regression coefficients seem reasonable; it makes sense that more wind would decrease temperature,
#but more sun and more ozone would increase temperature (greenhouse effect)
anova(mod1, mod2, mod3)
#Model 3 explains significantly more variance than the other two models.


#####EXERCISE 3
#compare AIC of mod1, mod2, mod3. Which model performs better?
AIC(mod1) #789.0
AIC(mod2) #782.7
AIC(mod3) #747.6
#Model 3 has the lowest AIC, indicating that model 3 allows the LEAST information to be lost. 
#Since model 3 has a better AIC by about 35, this is considered significantly better than the other models.


