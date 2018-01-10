###Practice 1
setwd("~/Desktop/LAB3")
install.packages("Lahman")
library(Lahman)
data("Salaries")
money14=subset(Salaries, yearID==2014)
hist(money14$salary)
avg=mean(money14$salary)
SD=sd(money14$salary)
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

### 10 submples with 50 values each
means= meansVector(10, 50, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 20 submples with 100 values each
means= meansVector(20, 100, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 40 submples with 100 values each
means= meansVector(40, 100, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 80 submples with 100 values each
means= meansVector(80, 100, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 160 submples with 100 values each
means= meansVector(160, 100, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 320 submples with 100 values each
means= meansVector(320, 100, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 640 submples with 100 values each
means= meansVector(640, 100, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 1280 submples with 100 values each
means= meansVector(1280, 100, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 1280 submples with 160 values each
means= meansVector(1280, 160, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 1280 submples with 640 values each
means= meansVector(1280, 640, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

### 1280 submples with 800 values each
means= meansVector(1280, 800, money14, "salary")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))




###Practice 2
data("mtcars")
hist(mtcars$mpg)
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

###10 submples with 20 values each
means= meansVector(10, 20, mtcars, "mpg")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

###50 submples with 20 values each
means= meansVector(50, 20, mtcars, "mpg")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

###100 submples with 20 values each
means= meansVector(100, 20, mtcars, "mpg")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

###100 submples with 5 values each
means= meansVector(100, 5, mtcars, "mpg")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

###100 submples with 10 values each
means= meansVector(100, 10, mtcars, "mpg")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

###100 submples with 20 values each
means= meansVector(100, 20, mtcars, "mpg")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

###100 submples with 30 values each
means= meansVector(100, 30, mtcars, "mpg")
hist(means, probability=TRUE)
lines(density(means, na.rm=TRUE), col="red")
avg=mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
