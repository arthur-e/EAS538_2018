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
mod2=lm(Temp~Ozone, data = airquality)
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
res=residuals(mod2)
RSS=sum(res^2)

RSE=sqrt(RSS/summary(mod2)$df[2])
#6.81
#6.81 equals the Residual Standard Error (RSE). To calculate this, you first need to calculate the residual. Then you take the sum of your residuals squared. Lastly, you take the square root of the residuals squared divided by the degrees freedom. 
#This is the square root of the MSE. 
#Essentially this is the standard deviation of the residuals. 
RMSE=RSS/116
RMSE
#45.69


MSE=RSS/summary(mod2)$df[2]
MSE2
#46.49
#To calculate the MSE, you take the sum of the squared residuals, divided by the degrees of freedom of mod3. 
#This shows the varriance of all residuals. 
