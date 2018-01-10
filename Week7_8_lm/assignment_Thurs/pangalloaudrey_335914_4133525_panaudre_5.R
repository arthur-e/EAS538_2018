data("airquality")
head(airquality, 15)
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
head(x, 20)
head(y1, 20)
head(y2, 20)
#I'm not sure why my numbers that come up with the head function are different from yours
# but it's working... that's something right?
cor(x, y1)
cor(x, y2)
plot(x, y1)
plot(x, y2)
#Exercise 1
data("faithful")
head(faithful)
pairs(faithful[c("eruptions", "waiting")])
cor(faithful[, c("eruptions", "waiting")]) #answer:0.9 regardless of which one is dependent or independent
plot(eruptions~waiting, data = faithful)
abline(lm(eruptions~waiting, data = faithful), col="red")

# That regression equation is terrifying
mod = lm(Temp~Wind, data=airquality)
summary(mod)
plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="violet")
#R doesn't recognize LILAC. Seriously...
#Exercise 2
funstuff = lm(Temp~Ozone, data=airquality)
summary(funstuff)
plot(Temp~Ozone, data = airquality)
abline(lm(Temp~Ozone, data = airquality), col="salmon")
#I can't believe R knows the color salmon but not lilac
res = residuals(mod)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod)$df[2])
RMSE = RSS/153
RMSE
MSE = RSS/summary(mod)$df[2]
MSE
anova(mod)
#Exercise 3
resfun= residuals(funstuff)
RSSFUN= sum(resfun^2)
RSEFUN= sqrt(RSSFUN/summary(funstuff)$df[2])
RMSEFUN= RSSFUN/153
RMSEFUN
MSEFUN= RSSFUN/summary(funstuff)$df[2]
MSEFUN
# I got 34.65 for RMS and 46.49 for MSE
anova(funstuff)
#looks like it worked? 