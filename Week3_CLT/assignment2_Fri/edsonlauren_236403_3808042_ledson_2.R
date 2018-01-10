# Lauren Edson, NRE538 Friday Lab 2

#setwd('C:/Users/Lauren/Documents/NRE 538')

##Functions
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

##Central Limit Theorem
install.packages("Lahman")
library(Lahman)
data("Salaries")

money14 = subset(Salaries, yearID==2014)
head(money14)

hist(money14$salary, main = "distribution of salary")
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))


###Exercise 1
means20 = meansVector(20, 100, money14, "salary")
means40 = meansVector(40, 100, money14, "salary")
means80 = meansVector(80, 100, money14, "salary")
means160 = meansVector(160, 100, money14, "salary")
means320 = meansVector(320, 100, money14, "salary")
means640 = meansVector(640, 100, money14, "salary")
means1280 = meansVector(1280, 100, money14, "salary")
hist(means20) 
hist(means40)
hist(means80)
hist(means160)
hist(means320)
hist(means640)
hist(means1280)


###Exercise 2
data(mtcars)
head(mtcars)

hist(mtcars$mpg)
hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#gradually increasing the number of subsamples
carmeans20 = meansVector(20, 100, mtcars, "mpg")
carmeans80 = meansVector(80, 100, mtcars, "mpg")
carmeans320 = meansVector(320, 100, mtcars, "mpg")
carmeans1280 = meansVector(1280, 100, mtcars, "mpg")
hist(carmeans20)
hist(carmeans80)
hist(carmeans320)
hist(carmeans1280)

#gradually increasing the number in each subsample
mpgmeans100 = meansVector(1280, 100, mtcars, "mpg")
mpgmeans160 = meansVector(1280, 160, mtcars, "mpg")
mpgmeans320 = meansVector(1280, 320, mtcars, "mpg")
mpgmeans640 = meansVector(1280, 640, mtcars, "mpg")
hist(mpgmeans100)
hist(mpgmeans160)
hist(mpgmeans320)
hist(mpgmeans640)


##Confidence Interval
# 
# library(Lahman)
# data(Batting)
# bat15 = subset(Batting, yearID==2015 & AB>200)
# bat15$avg = bat15$H/bat15$AB
# hist(bat15$avg, breaks=30, freq=FALSE)
# lines(density(bat15$avg), col="red")
# 
# avg.se = sd(bat15$avg)/sqrt(length(bat15$avg))
# ci.lo = mean(bat15$avg)-qnorm(.95)*avg.se
# ci.hi = mean(bat15$avg)+qnorm(.95)*avg.se
# 
# shapiro.test(bat15$avg)
# qqnorm(bat15$avg)
# qqline(bat15$avg, col="Red")
# 
# set.seed(61)
# avg.m=c()
# for (i in 1:10000){
#   avg.m[i]=mean(sample(bat15$avg, length(bat15$avg), replace=TRUE))
# }
# 
# mean.resamp=mean(avg.m)
# mean.SE.resamp=sd(avg.m)
# mean.CI.resamp=c(sort(avg.m)[10000*0.025], sort(avg.m)[10000*0.975])
# print(paste0("standard error calculated by resampling = ", round(mean.SE.resamp,5)))
# #[1] "standard error calculated by resampling = 0.00166"
# 
# print(paste0("95% confidence interval by resampling : ", round(sort(avg.m)[10000*0.025],4), " - ", round(sort(avg.m)[10000*0.975],4)))
# #[1] "95% confidence interval by resampling : 0.2579 - 0.2644"



