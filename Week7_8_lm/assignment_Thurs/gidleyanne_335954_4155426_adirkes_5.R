# NRE538
# adirkes

data("airquality") # to be used later in exercises

#Exercise 1
data("faithful")
#head(faithful,15)
cor(faithful[,c("eruptions","waiting")],use="na.or.complete")

#Exercise 2
ozmod = lm(Ozone~Wind, data=airquality)
summary(ozmod)
plot(Ozone~Wind, data=airquality)
abline(ozmod,col="blue")

ofmod = lm(eruptions~waiting,data=faithful)
summary(ofmod)
plot(eruptions~waiting, data=faithful)
abline(ofmod,col="red")
# yes, eruption time is positively correlated with wait time

#Exercise 3
res = residuals(ozmod)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(ozmod)$df[2])
RSE # 26.5
n = 153; # number of observations in data set
RMSE = RSS/n
#RMSE
MSE = RSS/summary(ozmod)$df[2]
MSE # 700.5
#anova(ozmod) # matches, 701

