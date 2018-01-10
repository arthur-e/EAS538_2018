## Assignment #2 by Chris Askew-Merwin

## Exercise #1: 5 histograms

library(Lahman)
data("Salaries")

money15 = subset(Salaries, yearID==2015)
head(money15)

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

### Histogram 1

hist(meansVector(10, 50, money15, "salary"), probability=TRUE, breaks = 19, main ="10 subsamples with 50 values each")
avg = mean(meansVector(10, 50, money15, "salary"))
SD = sd(meansVector(10, 50, money15, "salary"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(10, 50, money15, "salary")), col="red")

### Histogram 2

hist(meansVector(40, 100, money15, "salary"), probability=TRUE, breaks = 19, main ="40 subsamples with 100 values each")
avg = mean(meansVector(40, 100, money15, "salary"))
SD = sd(meansVector(40, 100, money15, "salary"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(40, 100, money15, "salary")), col="red")

### Histogram 3

hist(meansVector(320, 100, money15, "salary"), probability=TRUE, breaks = 19, main ="320 subsamples with 100 values each")
avg = mean(meansVector(320, 100, money15, "salary"))
SD = sd(meansVector(320, 100, money15, "salary"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(320, 100, money15, "salary")), col="red")

### Histogram 4

hist(meansVector(1280, 100, money15, "salary"), probability=TRUE, breaks = 19, main ="1280 subsamples with 100 values each")
avg = mean(meansVector(1280, 100, money15, "salary"))
SD = sd(meansVector(1280, 100, money15, "salary"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(1280, 100, money15, "salary")), col="red")

### Histogram 5

hist(meansVector(2560, 100, money15, "salary"), probability=TRUE, breaks = 19, main ="2560 subsamples with 100 values each")
avg = mean(meansVector(2560, 100, money15, "salary"))
SD = sd(meansVector(2560, 100, money15, "salary"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(2560, 100, money15, "salary")), col="red")



## Exercise #2: 5 histograms

data("mtcars")
hist(mtcars$mpg)

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


### Histogram 1

hist(meansVector(10, 50, mtcars, "mpg"), probability=TRUE, breaks = 19, main ="10 subsamples with 50 values each")
avg = mean(meansVector(10, 50, mtcars, "mpg"))
SD = sd(meansVector(10, 50, mtcars, "mpg"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(10, 50, mtcars, "mpg")), col="red")


### Histogram 2

hist(meansVector(40, 100, mtcars, "mpg"), probability=TRUE, breaks = 19, main ="10 subsamples with 50 values each")
avg = mean(meansVector(40, 100, mtcars, "mpg"))
SD = sd(meansVector(40, 100, mtcars, "mpg"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(40, 100, mtcars, "mpg")), col="red")


### Histogram 3

hist(meansVector(320, 100, mtcars, "mpg"), probability=TRUE, breaks = 19, main ="10 subsamples with 50 values each")
avg = mean(meansVector(320, 100, mtcars, "mpg"))
SD = sd(meansVector(320, 100, mtcars, "mpg"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(320, 100, mtcars, "mpg")), col="red")


### Histogram 4

hist(meansVector(1280, 100, mtcars, "mpg"), probability=TRUE, breaks = 19, main ="10 subsamples with 50 values each")
avg = mean(meansVector(1280, 100, mtcars, "mpg"))
SD = sd(meansVector(1280, 100, mtcars, "mpg"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(1280, 100, mtcars, "mpg")), col="red")


### Histogram 5

hist(meansVector(2560, 100, mtcars, "mpg"), probability=TRUE, breaks = 19, main ="10 subsamples with 50 values each")
avg = mean(meansVector(2560, 100, mtcars, "mpg"))
SD = sd(meansVector(2560, 100, mtcars, "mpg"))
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))
lines(density(meansVector(2560, 100, mtcars, "mpg")), col="red")

