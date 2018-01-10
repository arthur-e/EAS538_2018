#Exercise 1
data("faithful")
head(faithful,15)
cor(faithful[,c("eruptions","waiting")],use="na.or.complete")
#The correlation coefficient between eruption time and waiting time is 0.9008112
plot(faithful[,c("eruptions","waiting")])

#Exercise 2
data("airquality")
head(airquality,15)
mod=lm(Temp~Ozone,data=airquality)
plot(Temp~Ozone,data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")
summary(mod)
#The slope (regression coefficient) is 0.20081, Multiple R squre=0.4877
#The p-value is lower than 2.2e-16

data("faithful")
head(faithful,15)
mod1=lm(eruptions~waiting,data=faithful)
summary(mod1)
#the regression coefficient is 0.075628>0,Multiple R-squre=0.8115
#The p-value<2.2e-16. So this regression is signficant,
#when waiting time get longer, the eruption time get longers,too. 
plot(eruptions~waiting,data=faithful)
abline(lm(eruptions~waiting,data=faithful),col="green")

#Exercise 3
res=residuals(mod)
RSS=sum(res^2)
RSE=sqrt(RSS/summary(mod)$df[2])
RSE
#RSE=6.818914
RMSE=RSS/272
RMSE
#RMSE=19.48796
MSE = RSS/summary(mod)$df[2]
MSE
#MSE= 46.49759