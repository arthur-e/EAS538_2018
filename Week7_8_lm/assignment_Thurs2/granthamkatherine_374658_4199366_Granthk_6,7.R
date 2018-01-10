data("airquality")
head(airquality, 15)

#Correlation just lets you know how tightly they go hand in hand with one another, not how depent they are on one another
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])
cor(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], use="na.or.complete")

#For the correlation coefficent, it's hard to say what is good. It depends on your system. Think of it as percent change of one variable based on another


#1.3 Linear Association ONLY

set.seed(15165)
f = function(x){
  y=-(x+3)^2 + runif(length(x), min=-2, max=2)
  #print(y)
}
f1 = function(x){
  y=runif(length(x), min=min(x), max=max(x))
}

x=seq(from=-6, to=0, by =0.005)
y1=f(x)
y2=f1(x)

head(x, 20)
head(y1,20)
head(y2, 20)

cor(x, y1)
#2.843496e-05
cor(x, y2)
#0.03969688

plot(x, y1)
plot(x, y2)

#Exercise 1
cor(faithful[,c("eruptions", "waiting")])
plot(faithful$eruptions, faithful$waiting)

#From running cor, we see that there is a positive correlation between eruptions and waiting. The correlation is strong due to the fact that the correlation is 0.9. This is close to 1, and so we can assume that the correlation between these variables is strong. 

#2.1 Linear Model
mod = lm(Temp~Wind, data=airquality)
summary(mod)

#(Dependentvariable~Independentvariable)
plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")

#Exercise 2
#State the intercept and slope and state whetehr significant
a2=airquality[which(is.na(airquality$Ozone)==FALSE),]
mod2=lm(Temp~Ozone, data = a2)
summary(mod2)
plot(Temp~Ozone, data = airquality)
abline(lm(Temp~Ozone, data = airquality), col="red")
#Here, the intercept is 69.41, and the slope is 0.2. From running the summary, we see that the p-value is significant. This means temperature and ozone are correlated. As Ozone changes, then we would see temperature change. We also see an Rsquared value of 0.4, which is fairly high and could potentially mean significance in the field of ecology. 

mod3=lm(eruptions~waiting, data=faithful)
summary(mod3)
plot(eruptions~waiting, data = faithful)
abline(lm(eruptions~waiting, data = faithful),col="red")
#Here, the intercept is -1.87, and the slope is 0.07. From running the summary, we see that there is a correlation between eruptions and waiting. We see that the relationship is significant due to the p-value being 2.2e-16, which is far under 0.05. We also see that the Rsquared value is 0.8115, which is fairly high and could mean potential signiciance. For me, getting this number would mean that there is a significant correlation here. 

#2.2 Errors
res = residuals(mod)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod)$df[2])
str(airquality)
str(summary(mod))
RMSE = RSS/153
RMSE
MSE = RSS/summary(mod)$df[2]
MSE
anova(mod)

#Exercise 3
res.0=residuals(mod2)
RSS.0=sum(res.0^2)

RSE.0=sqrt(RSS.0/summary(mod2)$df[2])
#6.81
#6.81 equals the Residual Standard Error (RSE). To calculate this, you first need to calculate the residual. Then you take the sum of your residuals squared. Lastly, you take the square root of the residuals squared divided by the degrees freedom. 
#This is the square root of the MSE. 
#Essentially this is the standard deviation of the residuals. 
RMSE.0=RSS.0/116
RMSE.0
#45.69


MSE.0=RSS.0/summary(mod2)$df[2]
MSE.0
#46.49
#To calculate the MSE, you take the sum of the squared residuals, divided by the degrees of freedom of mod3. 
#This shows the varriance of all residuals. 

SSY = deviance(lm(a2$Temp~1))
SSY1 = var(airquality$Temp)*152
R2 = 1 - RSS/SSY
SSY
#13617.88
SSY1
#13617.88
R2
#0.209

#Exercise 4
RSS2=RSS.0/114
SSY2=SSY/115

R2A = 1 - (RSS2/SSY2)
R2A

#0.483

plot(residuals(mod))

install.packages('lmtest')
library(lmtest)
dwtest(mod, alternative=c("two.sided"))

plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red")

install.packages('lmtest')
library(lmtest)
bptest(mod)

qqnorm(residuals(mod))
qqline(residuals(mod), col="red")

shapiro.test(residuals(mod))

plot(mod)

#Exercise 5
dwtest(mod2, alternative = c("two.sided"))
#The dwtest tests for autocorrelation, which you want to happen with your data. Based on the results calculated about, we recieve a DW= 1.1833 and  p-value of 7.27e-06. We have a signficant p-value, since it is less that 0.05, we have a very low risk of commiting a type 1 error. Therefore, the data has auto correlation, and we reject the null hypothesis. 

bptest(mod2)
#The bptest is used to calculate examine homoscedasticity. From this test, we recieved a p-value of 0.07, which is very close to be homoscedastic, but it is not quite. This is because the p-value is greater than 0.05, so we fail to reject the null because we run the risk of committing error. 

qqnorm(residuals(mod2))
qqline(residuals(mod2), col="red")
shapiro.test(residuals(mod2))
#From this data set, we see that the p-value is 0.000226, which is significant. This is because it is less than 0.05, and so we run a very small risk of committing error. Therefore, we assume normality. We can also see this visually using the QQ- Plot, in which the data is fairly normally distributed. 