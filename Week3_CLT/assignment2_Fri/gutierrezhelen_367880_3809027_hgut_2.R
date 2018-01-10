setwd("C:/Users/helen/Documents/winter_2017/538_stats/lab/W3")

#EXERCISE 1
library("Lahman")
data("Salaries")

money14=subset(Salaries, yearID==2014)
head(money14)

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

hist(money14$salary, probability=TRUE, main = "Distribution of Salary")
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#10 Subsamples with 50 values each
meansVector(10, 50, money14, "salary")

hist(meansVector(10, 50 , money14, "salary"), probability=TRUE, main = "10 subsamples with 50 values each", breaks=10)
lines(density(meansVector(10, 50 , money14, "salary"), na.rm=TRUE), col="red")
SD=sd(meansVector(10, 50 , money14, "salary"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#50 Subsamples with 200 values each
meansVector(50, 200, money14, "salary")

hist(meansVector(50, 200, money14, "salary"), probability=TRUE, main = "50 subsamples with 200 values each", breaks=20)
lines(density(meansVector(50, 200, money14, "salary"), na.rm=TRUE), col="red")
SD=sd(meansVector(50, 200, money14, "salary"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#50 Subsamples with 800 values each
meansVector(50, 800, money14, "salary")

hist(meansVector(50, 800, money14, "salary"), probability=TRUE, main = "50 subsamples with 800 values each", breaks=50)
lines(density(meansVector(50, 800, money14, "salary"), na.rm=TRUE), col="red")
SD=sd(meansVector(50, 800, money14, "salary"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#50 Subsamples with 1600 values each
meansVector(50, 1600, money14, "salary")

hist(meansVector(50, 1600, money14, "salary"), probability=TRUE, main = "50 subsamples with 1600 values each", breaks=50)
lines(density(meansVector(50, 1600, money14, "salary"), na.rm=TRUE), col="red")
SD=sd(meansVector(50, 1600, money14, "salary"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#50 Subsamples with 2400 values each
meansVector(50, 2400, money14, "salary")

hist(meansVector(50, 2400, money14, "salary"), probability=TRUE, main = "50 subsamples with 2400 values each", breaks=15, xlim=c(3000000, 5000000))
lines(density(meansVector(50, 2400, money14, "salary"), na.rm=TRUE), col="red")
SD=sd(meansVector(50, 2400, money14, "salary"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#200 Subsamples with 2400 values each
meansVector(200, 2400, money14, "salary")

hist(meansVector(200, 2400, money14, "salary"), probability=TRUE, main = "200 subsamples with 2400 values each", breaks=15, xlim=c(3000000, 5000000))
lines(density(meansVector(200, 2400, money14, "salary"), na.rm=TRUE), col="red")
SD=sd(meansVector(200, 2400, money14, "salary"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#800 Subsamples with 2400 values each
meansVector(800, 2400, money14, "salary")

hist(meansVector(800, 2400, money14, "salary"), probability=TRUE, main = "800 subsamples with 2400 values each", breaks=15, xlim=c(3000000, 5000000))
lines(density(meansVector(800, 2400, money14, "salary"), na.rm=TRUE), col="red")
SD=sd(meansVector(800, 2400, money14, "salary"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#1600 Subsamples with 2400 values each
meansVector(1600, 2400, money14, "salary")

hist(meansVector(1600, 2400, money14, "salary"), probability=TRUE, main = "1600 subsamples with 2400 values each", breaks=15, xlim=c(3000000, 5000000))
lines(density(meansVector(1600, 2400, money14, "salary"), na.rm=TRUE), col="red")
SD=sd(meansVector(1600, 2400, money14, "salary"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#2400 Subsamples with 2400 values each
meansVector(2400, 2400, money14, "salary")

hist(meansVector(2400, 2400, money14, "salary"), probability=TRUE, main = "2400 subsamples with 2400 values each", breaks=15, xlim=c(3000000, 5000000))
lines(density(meansVector(2400, 2400, money14, "salary"), na.rm=TRUE), col="red")
SD=sd(meansVector(2400, 2400, money14, "salary"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#EXERCISE 2

datasets::mtcars

hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

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

#10 Subsamples with 50 values each
meansVector(10, 50, mtcars, "mpg")

hist(meansVector(10, 50, mtcars, "mpg"), probability=TRUE, main = "10 subsamples with 50 values each", breaks=20)
lines(density(meansVector(10, 50, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(10, 50, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#50 Subsamples with 150 values each
meansVector(50, 150, mtcars, "mpg")

hist(meansVector(50, 150, mtcars, "mpg"), probability=TRUE, main = "50 subsamples with 150 values each", breaks=20)
lines(density(meansVector(50, 150, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(50, 150, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#50 Subsamples with 500 values each
meansVector(50, 500, mtcars, "mpg")

hist(meansVector(50, 500, mtcars, "mpg"), probability=TRUE, main = "50 subsamples with 500 values each", breaks=20)
lines(density(meansVector(50, 500, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(50, 500, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#50 Subsamples with 1000 values each
meansVector(50, 1000, mtcars, "mpg")

hist(meansVector(50, 1000, mtcars, "mpg"), probability=TRUE, main = "50 subsamples with 1000 values each", breaks=20)
lines(density(meansVector(50, 1000, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(50, 1000, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#50 Subsamples with 2000 values each
meansVector(50, 2000, mtcars, "mpg")

hist(meansVector(50, 2000, mtcars, "mpg"), probability=TRUE, main = "50 subsamples with 2000 values each", breaks=20)
lines(density(meansVector(50, 2000, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(50, 2000, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#50 Subsamples with 3000 values each
meansVector(50, 3000, mtcars, "mpg")

hist(meansVector(50, 3000, mtcars, "mpg"), probability=TRUE, main = "50 subsamples with 3000 values each", breaks=20)
lines(density(meansVector(50, 3000, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(50, 3000, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#150 Subsamples with 3000 values each
meansVector(150, 3000, mtcars, "mpg")

hist(meansVector(150, 3000, mtcars, "mpg"), probability=TRUE, main = "150 subsamples with 3000 values each", breaks=50)
lines(density(meansVector(150, 3000, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(150, 3000, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#500 Subsamples with 3000 values each
meansVector(500, 3000, mtcars, "mpg")

hist(meansVector(500, 3000, mtcars, "mpg"), probability=TRUE, main = "500 subsamples with 3000 values each", breaks=20, xlim=c(19,21))
lines(density(meansVector(500, 3000, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(500, 3000, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#1000 Subsamples with 3000 values each
meansVector(1000, 3000, mtcars, "mpg")

hist(meansVector(1000, 3000, mtcars, "mpg"), probability=TRUE, main = "1000 subsamples with 3000 values each", breaks=20, xlim=c(19,21))
lines(density(meansVector(1000, 3000, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(1000, 3000, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#2000 Subsamples with 3000 values each
meansVector(2000, 3000, mtcars, "mpg")

hist(meansVector(2000, 3000, mtcars, "mpg"), probability=TRUE, main = "2000 subsamples with 3000 values each", breaks=20, xlim=c(19,21))
lines(density(meansVector(2000, 3000, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(2000, 3000, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#3000 Subsamples with 3000 values each
meansVector(3000, 3000, mtcars, "mpg")

hist(meansVector(3000, 3000, mtcars, "mpg"), probability=TRUE, main = "3000 subsamples with 3000 values each", breaks=20, xlim=c(19,21))
lines(density(meansVector(3000, 3000, mtcars, "mpg"), na.rm=TRUE), col="red")
SD=sd(meansVector(3000, 3000, mtcars, "mpg"))
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
