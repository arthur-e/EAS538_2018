#EX1
airquality1 = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE) 
pairs(airquality1[,c(1:4)])

cor(airquality1[,c(1:4)], use="na.or.complete")
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.1, max=20)
X2 = (Y+3)/2 + runif(length(Y), min=0.1, max=15)
pairs(airquality1[,c(1:4)])
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))
plot(X1~X2)
#Through increasing random noise in both X1 and X2 I was able to decrease their codependency, such that correlation between the two explanatory variables is greatly reduced.
#This is an important to make accurate predictions of y given true x values. (To have high confidence in parameter coefficients)

#EX2
mod1 = lm(Temp~Wind, data=airquality1)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality1)
summary(mod2)
#BONUS:
res1 = residuals(mod1)
RSS1 = sum(res1^2)
res2 = residuals(mod2)
RSS2 = sum(res2^2)
#1
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality1)
summary(mod3)
#Adjusted R-squared of mod3= 0.4858, compared to O.2403 of model 1 and 0.2884 of model 2.
anova(mod2, mod3)
#Model 3 performs significantly better than model 2 because it reduces residual sum of squares (RSS). Therefore this model captures more variability in y than model 2 (which peformed better than model 1). Because the Adjusted R^2 was lowest for model 3, we can conclude that even taking into account the extra parameter (Ozone), model 3 is the most robust. 

#EX3
AIC(mod1, mod2, mod3)
#Model 3 has the lowest AIC (a difference >10), and thus reduces information losses (considering number of parameters) compared to models 1 and 2 (which differ by a potentially insignifican <10 AIC value).Therefore model 3 performs best.
