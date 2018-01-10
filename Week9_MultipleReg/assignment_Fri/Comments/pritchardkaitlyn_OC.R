##EX 1
library(dplyr)
aq = airquality %>%
  subset(is.na(Solar.R) == FALSE & is.na(Ozone)==FALSE)

pairs(aq[,c(1:4)])
Y=seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=15)
# X1 to Y model: the regression coefficient of X1 is 0.52 and the r2 is 0.18
X2= (Y+3)/2+runif(length(Y), min=0.5, max=15)
# X2 to Y model: the regression coefficient of X2 is 0.66 and the r2 is 0.28.
df=as.data.frame(cbind(Y, X1, X2))
# The true regression coefficients are: X1 = 0.39 and X2 = 0.57
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
summary(lm(Y~X1+X2, data=df))
plot(X1~X2)
cor(X1, X2)
#When I increased the amount of noise in both the X1 and X2 equations by increasing the max to 15, the correlation between the two variables dropped to 0.27.
#So if we look at X1 and X2 when both are included in the regression model, the regression coefficients for both are closer to the true values and not far off like they were when X1 and X2 were highly correlated
#My R2 also increased slightly, since it was considering two variables instead of 1.

##EX 2 BONUS
mod1 = lm(Temp~Wind, data=aq)
summary(mod1)
res1=residuals(mod1)
RSS1=sum((res1)^2)
RSS1  # RSS = 7520.672

mod2 = lm(Temp~Wind+Solar.R, data=aq)
summary(mod2)
res2=residuals(mod2)
RSS2=sum((res2)^2)
RSS2  # RSS = 6979.506
anova(mod1, mod2)

numerator = (RSS1-RSS2)/(2-1)
denominator = RSS2/(110-2)
F = numerator/denominator
F  # F = 8.37

pf = pf(q=8.37,df1=1,df2=108,lower.tail=FALSE)
pf #pf = 0.0046

#EX 2 Pt. 1
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=aq)
summary(mod3)
anova(mod1, mod2)
# Since mod2's p<0.05, mod2 explains a significantly greater amount of variance compared to mod1
anova(mod1, mod2, mod3)
# Since mod3's p<0.05 and is smaller than mod2's p value, mod 3 explains a significantly different amount of variance compared to mod1 and mod2

##### Oscar: P-value in F-test is not interpreted this way. 
#####        The correct interpretation should be that the probability for model 1 to explain the same amount of variance 
#####        as model 3 does is 2.424e-09, which is so low that it is very unlikely due to random processes (coincidence).
#####        We therefor state that model 3 "significantly" explain more variance than model 1.


vif.rad = 1/(1 - summary(lm(Temp~Wind+Solar.R+Ozone, dat=aq))$r.squared)
vif.rad #1.999
# Since our VIF is less than 3, there is not much collinearity and therefore we have reasonable estimates of our regression coefficients
# Overall, mod3 does explain more variance than mod1 or mod2

#EX 3
aic1=AIC(mod1)
aic1  #aic1 = 788.97
aic2=AIC(mod2)
aic2 #aic2 = 782.68
aic3=AIC(mod3)
aic3 #aic3 = 747.58
# Since model 3 has the lowest AIC value, I would conclude that it is the best performing model
# Since there is more than a difference of 10 between the AIC values of model 3 v. model 1 or 2, it does perform significantly better than either of those models

##### Oscar: From AIC, the better model means the one that has higher probability to regenerate the data we observed. 
#####        Also, AIC comparison never does significant test. 
#####        We can NOT say any model is "significantly" better than others based on AIC difference.
