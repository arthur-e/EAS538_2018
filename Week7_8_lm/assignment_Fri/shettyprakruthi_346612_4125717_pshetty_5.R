data("airquality")
head(airquality, 15)

#Exercise 1
data("faithful")
head(faithful)
cor(faithful$eruptions,faithful$waiting) #
plot(faithful$eruptions,faithful$waiting)


#Exercise2
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")


modnew = lm(eruptions~waiting, data=faithful)
summary(modnew)
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")
#Yes, Longer eruptions occur with longer wait time. The plot as well as the waiting coefficient estimate of 0.075 signify it (positive value indicating dependent variable value increases with in crease in independent variable value)

#Exercise3
modnew1=lm(Temp~Ozone, data=airquality)
res = residuals(modnew1)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(modnew1)$df[2])
str(airquality)
RSE#Residual standard error: 6.818914
RMSE = RSS/153
MSE = RSS/summary(modnew1)$df[2]
MSE#Mean square error: 46.49759
anova(modnew1)




#### Lab

pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])

cor(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], use="na.or.complete")

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

cor(y1,x)
cor(y2,x)

plot(y1,x)
plot(y2,x)


mod = lm(Temp~Wind, data=airquality)
summary(mod)
plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")
