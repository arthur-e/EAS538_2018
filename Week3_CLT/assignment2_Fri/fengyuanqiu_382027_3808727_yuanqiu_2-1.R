install.packages("Lahman")
library(Lahman)
data("Salaries")

##Meansvector function

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

#Exercise 1

salaries2014 = subset(Salaries, yearID == 2014)
head(salaries2014)

hist(salaries2014$salary, main = "Distribution of Salaries 2014")
avg=mean(salaries2014$salary)
SD=sd(salaries2014$salary)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))


#As number of samples increases,

hist(meansVector(10, 50, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(10, 50, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(20, 100, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(20, 100, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(40, 100, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(40, 100, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(80, 100, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(80, 100, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(160, 100, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(160, 100, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(320, 100, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(320, 100, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(640, 100, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(640, 100, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(1280, 100, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 100, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(2560, 100, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(2560, 100, salaries2014, "salary"), na.rm=TRUE), col="red")


#As sample size increases,

hist(meansVector(1280, 10, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 10, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(1280, 20, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 20, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(1280, 40, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 40, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(1280, 80, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 80, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(1280, 160, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 160, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(1280, 320, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 320, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(1280, 640, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 640, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(1280, 1280, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 1280, salaries2014, "salary"), na.rm=TRUE), col="red")

hist(meansVector(1280, 2560, salaries2014, "salary"), probability = TRUE, xlim=range(2000000:8000000), breaks = 10)
lines(density(meansVector(1280, 2560, salaries2014, "salary"), na.rm=TRUE), col="red")



##Exercise 2

data("mtcars")
hist(mtcars$mpg)

hist(mtcars$mpg, main = "Distribution of Miles per Gallon", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

##Meansvector function

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



#As number of samples increases,


hist(meansVector(20, 100, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(20, 100, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(40, 100, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(40, 100, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(80, 100, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(80, 100, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(160, 100, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(160, 100, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(320, 100, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(320, 100, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(640, 100, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(640, 100, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(1280, 100, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 100, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(2560, 100, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(2560, 100, mtcars, "mpg"), na.rm=TRUE), col="red")


#As sample size increases,

hist(meansVector(1280, 10, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 10, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(1280, 20, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 20, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(1280, 40, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 40, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(1280, 80, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 80, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(1280, 160, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 160, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(1280, 320, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 320, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(1280, 640, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 640, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(1280, 1280, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 1280, mtcars, "mpg"), na.rm=TRUE), col="red")

hist(meansVector(1280, 2560, mtcars, "mpg"), probability = TRUE, xlim=range(18:23), breaks = 10)
lines(density(meansVector(1280, 2560, mtcars, "mpg"), na.rm=TRUE), col="red")

