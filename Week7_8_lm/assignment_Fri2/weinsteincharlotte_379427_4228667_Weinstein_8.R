#Lab 8

####EXERCISE 4

data("airquality") #load air quality data

mod.ex4 = lm(Temp~Ozone, data=airquality) #make linear model
res.ex4 = residuals(mod.ex4) #calculate residuals
RSS.ex4 = sum(res.ex4^2) #calculate RSS
k = 2 #2 parameters
n = length(res.ex4) #n = 116 because res.ex4 has 116 values (after removing NAs from Temp and/or Ozone data)
#SSY = sum((y-ymean)^2); can calculate by doing deviance of null model after removing NAs
dat.new = subset(airquality, Ozone!="NA" & Temp!="NA") #remove NAs from data to prep for SSY calculation
SSY.ex4 = deviance(lm(Temp~1, data=dat.new)) #calculate SSY using null model with rows with NAs removed
adjR2 = 1 - (RSS.ex4/(n-k))/(SSY.ex4/(n-1))
adjR2
summary(mod.ex4) #check our R2 using the model summary
#adjusted R2 = 0.4832


#rewrite this code using variance:
SSY1.ex4 = var(dat.new$Temp)*115 #this should be equal to SSY.ex4
#114 = degrees of freedom of residuals
#115 = degrees of freedom of the sample
newR2 = 1 - (RSS.ex4/(n-k))/(SSY1.ex4/(n-1))
newR2
#newR2 = 0.4832, which is equal to the above previous calculation and the computer-calculated value.

#### EXERCISE 5

model = lm(Ozone~Wind, data=airquality)
plot(model)
#this plot function generates graphs of the residuals, a qqplot, and square root of residuals,
#which can be used to check for independency, homoscedasticity, and normality (ignoring fourth graph).

library(lmtest)
dwtest(model, alternative=c("two.sided"))
#This test confirms that there is no lack of dependency, as our p-value is less than .05

bptest(model)
#This tests for homoscedasticity. Our p-value is less than .05, so our residuals are not homoscedastistic.

qqnorm(residuals(model))
qqline(residuals(model), col="red")
#The plot stays close to the redline, indicating that our residuals are normally distributed.

shapiro.test(residuals(model))
#However, our p-value here is less than .05, indicating that the residuals are not normally distributed.
#This would make me decide that our residuals are not normally distributed, despite the q-q plot.