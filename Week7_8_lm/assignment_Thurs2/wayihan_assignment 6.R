#Assignemnt 6 Exercise 4
data("airquality")
head(airquality, 15)
Ozone=subset(airquality, is.na(Ozone)==FALSE&is.na(Temp)==FALSE, data=airquality)
model=lm(Temp~Ozone, data=Ozone)
summary(model)
res=residuals(model)
head(res)
RSS=sum(res^2)
k=2
RSS1 = sum(res^2)/(116-2)
RSS1
SSY1 = var(Ozone$Temp)*115/115
SSY1 
R2 = 1-RSS1/SSY1
R2
#Adjusted R2 is 0.4832134

#Exercise 5
#Test residual independency
plot(residuals(model))
#The residuals show a temporal pattern
install.packages("lmtest")
library(lmtest)
dwtest(model, alternative=c("two.sided"))
#The dwtest shows there is autocorrelation among residuals. If the residuals are independent, then there should not be a correlation.

#Test residual homoscedasticity
plot(residuals(model)~fitted(model))
abline(lm(residuals(model)~fitted(model)), col="red")
#The variance are not evenly distributed in the plot, which means residuals are not homoscedastic
#bptest
library(lmtest)
bptest(model)
#p value is 0.0721, we fail to reject null hypothesis, which means residuals are homoscedastic

#Test residuals' normal distribution
qqnorm(residuals(model))
qqline(residuals(model), col="red")
#From the QQ plot, we can see that the residuals are not normally distributed
shapiro.test(residuals(model))
#p value is less than 0.000226, we reject the null hypothesis, which reinforce the assumption that
#the residuals are not normally distributed. 

