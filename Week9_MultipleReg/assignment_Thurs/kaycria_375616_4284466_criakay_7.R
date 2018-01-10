##Ex. 1##
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=10, max=45)
X2 = (Y+3)/2 + runif(length(Y), min=10, max=45)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
#Coorelation Coefficent for x1 is 0.07825
summary(lm(Y~X2, data=df))
#Coorelation Coefficent for x2 is 0.18177
summary(lm(Y~X1+X2, data=df))
#Coorelation Coefficient for x1 is 0.04466
#Coorelation Coefficient for x2 is 0.17301
plot(X1~X2)
cor(X1, X2)
#More random noise results in less coorelation. 
#The correlation coefficent for cor(X1, X2) goes down to 0.1951297 
#(below 5 generally indicates that they are not coorelated)


##Ex. 2##
mod1 = lm(Temp~Wind, data=airquality)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)

mod3= lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod1,mod2,mod3)
#It does indicate that there is more variance because 
#there is significantly lower RSS (shown through a very 
#small p-value and higher f-statistic)


##Ex. 3##
AIC(mod1)
#AIC of mod1 = 788.9671
AIC(mod2)
#AIC of mod2 = 782.6779
AIC(mod3)
#AIC of mod3 = 747.5795
#Comparing AIC between the three models also show 
#that model three is the best (the AIC is lowest, 
#meaning that probability is highest)