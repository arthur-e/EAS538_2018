###Lab set-up
setwd("~/Desktop/Stats R/Stats")
install.packages("Lahman")
library(Lahman)
data("Salaries")
###
###Lab demonstrations
money15<-subset(Salaries, yearID==2015)
head(money15)
hist(money15$salary)
hist(money15$salary, main = "distribution of salary")
avg=mean(money15$salary)
SD=sd(money15$salary)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
meansVector = function(times, size, dat, varb){ #Resampling function
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
###Exercise 1-Varying numbers of subsamples
money14<-subset(Salaries, yearID==2014)
means1<-meansVector(10, 100, money14, "salary")
hist(means1, main="10 Subsamples with 100 Values Each")
means2<-meansVector(20, 100, money14, "salary")
hist(means2, main="20 Subsamples with 100 Values Each")
means3<-meansVector(80, 100, money14, "salary")
hist(means3, main="80 Subsamples with 100 Values Each")
means4<-meansVector(320, 100, money14, "salary")
hist(means4, main="320 Subsamples with 100 Values Each")
means5<-meansVector(1280, 100, money14, "salary")
hist(means5, main="1280 Subsamples with 100 Values Each")
#Varying numbers of values
means6<-meansVector(1280, 50, money14, "salary")
hist(means6, main="1280 Subsamples with 50 Values Each")
means7<-meansVector(1280, 150, money14, "salary")
hist(means7, main="1280 Subsamples with 150 Values Each")
means8<-meansVector(1280, 200, money14, "salary")
hist(means8, main="1280 Subsamples with 200 Values Each")
means9<-meansVector(1280, 500, money14, "salary")
hist(means9, main="1280 Subsamples with 500 Values Each")
means10<-meansVector(1280, 1000, money14, "salary")
hist(means10, main="1280 Subsamples with 1000 Values Each")
###
###Exercise 2-Varying numbers of subsamples
data("mtcars")
car_mpg<-subset(mtcars, select=mpg)
car1<-meansVector(10, 100, car_mpg, "mpg")
hist(car1, main="10 Subsamples with 100 Values Each")
car2<-meansVector(20, 100, car_mpg, "mpg")
hist(car2, main="20 Subsamples with 100 Values Each")
car3<-meansVector(80, 100, car_mpg, "mpg")
hist(car3, main="80 Subsamples with 100 Values Each")
car4<-meansVector(320, 100, car_mpg, "mpg")
hist(car4, main="320 Subsamples with 100 Values Each")
car5<-meansVector(1280, 100, car_mpg, "mpg")
hist(car5, main="1280 Subsamples with 100 Values Each")
#Varying numbers of values
car6<-meansVector(1280, 50, car_mpg, "mpg")
hist(car6, main="1280 Subsamples with 50 Values Each")
car7<-meansVector(1280, 150, car_mpg, "mpg")
hist(car7, main="1280 Subsamples with 150 Values Each")
car8<-meansVector(1280, 200, car_mpg, "mpg")
hist(car8, main="1280 Subsamples with 200 Values Each")
car9<-meansVector(1280, 500, car_mpg, "mpg")
hist(car9, main="1280 Subsamples with 500 Values Each")
car10<-meansVector(1280, 1000, car_mpg, "mpg")
hist(car10, main="1280 Subsamples with 1000 Values Each")
###
###Other lab demonstrations
hist(mtcars$mpg)
hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
# It's a pretty flat distribution (uniform distribution)

library(Lahman)
data(Batting)
bat15 = subset(Batting, yearID==2015 & AB>200)
bat15$avg = bat15$H/bat15$AB
hist(bat15$avg, breaks=30, freq=FALSE)
lines(density(bat15$avg), col="red")
shapiro.test(bat15$avg)
qqnorm(bat15$avg); qqline(bat15$avg, col="Red")
set.seed(61)
avg.m=c()
for (i in 1:10000){
  avg.m[i]=mean(sample(bat15$avg, length(bat15$avg), replace=TRUE))
}

mean.resamp=mean(avg.m)
mean.SE.resamp=sd(avg.m)
mean.CI.resamp=c(sort(avg.m)[10000*0.025], sort(avg.m)[10000*0.975])
print(paste0("standard error calculated by resampling = ", round(mean.SE.resamp,5)))
print(paste0("95% confidence interval by resampling : ", 
             round(sort(avg.m)[10000*0.025],4), " - ", round(sort(avg.m)[10000*0.975],4)))

