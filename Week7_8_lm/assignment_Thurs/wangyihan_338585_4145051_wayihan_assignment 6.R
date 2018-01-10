#Assignment 6
#Exercise 1
data("faithful")
head(faithful)
cor(faithful[, c("eruptions", "waiting")])
plot(faithful)

#Exercise 2
data("airquality")
head(airquality)

model=lm(Temp~Ozone, data=airquality)
summary(model)
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")
#The estimated regression line is shown in red, which is y=0.02*x+69.41

model=lm(eruptions~waiting, data=faithful)
summary(model)
#From coefficients table, we can see the slope of the correlation is 0.076. It is a positive number which means there is a positive correlation between eruptions time and waiting time. It also means that if the waitng time increase by 1 unit the duration of the eruption will increase by 0. 075628
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="green")

#Exercise 3
res=residuals(model)
head(res)
RSS=sum(res^2)
RSE=sqrt(RSS/summary(model)$df[2])
RSE
RMSE=RSS/272
RMSE
MSE=RSS/summary(model)$df[2]
MSE
# Residual standard error(RSE) equals 6.818914
# Mean square error (MSE) equals 46.49759

#Anova table to confirm MSE
anova(model)

#The mean Sq is 46.5 in anova table. Answer confirmed 
