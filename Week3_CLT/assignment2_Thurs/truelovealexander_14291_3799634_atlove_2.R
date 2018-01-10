install.packages("Lahman")
library(Lahman)
data("salaries")

money15 = subset(Salaries, yearID==2015)

head(money15)

hist(money15$salary, main = "distribution of salary")
avg=mean(money15$salary)
SD=sd(money15$salary)
abline(v=avg, col="orange")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("orange", "blue"))

##^^not a normal distribution##

##meansVector is just a function, no output##

meansVector = function(times, size, dat, varb){
  a = as.numeric(times)
  b = as.numeric(size)
  v = c()
  for(i in 1:a){
    y = sample(dat[,varb],b,replace=TRUE)
    m = mean(y)
    v = c(v,m)
  }
  v
}

###Exercise 1###
#number of samples, how many values in each, dataset/subset, "variable"##

money14 = subset(Salaries, yearID==2014)

meansVector(10, 5, money14, "salary")
Vector1 = meansVector(10, 5, money14, "salary")
hist(Vector1)

Vector1 = meansVector(1000, 50, money14, "salary")
hist(Vector1)
abline(v=avg, col="orange")

Vector1 = meansVector(5000, 1200, money14, "salary")
hist(Vector1)
abline(v=avg, col="orange")

Vector1 = meansVector(1280, 2560, money14, "salary")
hist(Vector1)
abline(v=avg, col="orange")

Vector1 = meansVector(400, 2560, money14, "salary")
hist(Vector1)
abline(v=avg, col="orange")

###Exercise 2###
#new dataset#

data(mtcars)
str(mtcars)
mtcars$mpg

hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "orange"))

#increasing samples#

Vector2 = meansVector(10, 5, mtcars, "mpg")
hist(Vector2)

Vector2 = meansVector(100, 5, mtcars, "mpg")
hist(Vector2)

Vector2 = meansVector(10000, 5, mtcars, "mpg")
hist(Vector2)

#increasing values#

Vector2 = meansVector(1000, 50, mtcars, "mpg")
hist(Vector2)

Vector2 = meansVector(1000, 500, mtcars, "mpg")
hist(Vector2)

Vector2 = meansVector(1000, 5000, mtcars, "mpg")
hist(Vector2)
abline(v=avg, col="blue")

#examine "for loop' which makes multiple histograms at once
#maybe: xlim = c(low, high) command sets x axis scale, to narrow or widen graph##

###Confidence Intervals and Standard Error###

library(Lahman)
data(Batting)
bat15 = subset(Batting, yearID==2015 & AB>200)
bat15$avg = bat15$H/bat15$AB
hist(bat15$avg, breaks=30, freq=FALSE)
lines(density(bat15$avg), col="red")

sd = sd(bat15$avg)
se = sd/sqrt(length(bat15$avg))
CI.lo = mean(bat15$avg) - se*(qnorm(0.975))
CI.hi = mean(bat15$avg) + se*(qnorm(0.975))

#can also use se*abs(qnorm(0.025))#

##vv a more complicated way to display a confidence interval, using the resampling method##

set.seed(61)
avg.m=c()
for (i in 1:10000){
  avg.m[i]= mean(sample(bat15$avg, length(bat15$avg), replace=TRUE))
}

mean.resamp=mean(avg.m)
mean.SE.resamp=sd(avg.m)
mean.CI.resamp=c(sort(avg.m)[10000*0.025], sort(avg.m)[10000*0.975])
print(paste0("standard error calculated by resampling = " round(mean.SE.resamp,5)))
print(paste0("95% confidence interval by resampling : ",
            round(sort(avg.m)[10000*0.025],4), " - ", round(sort(avg.m)[10000*0.975],4)))



