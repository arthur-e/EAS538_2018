install.packages("Lahman")
library(Lahman)
data("Salaries")

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

#####EXERCISE 1

money14 = subset(Salaries, yearID==2014)

#distribution of salary histogram
hist(money14$salary, main = "distribution of salary")
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))


#10 subsamples with 50 values each
means = meansVector(10, 50, money14, "salary")
hist(means, freq=FALSE, xlim=c(1e+06,7e+06), main = "10 subsamples with 50 values each")
avg = mean(means)
SD=sd(means)
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
lines(density(means, na.rm=TRUE), col="red")

#vector of numbers of subsamples for 100 values each
vars = c(20, 40, 80, 160, 320, 640, 1280, 2560)

for(time in vars){
  means= meansVector(time, 100, money14, "salary")
  hist(means, freq=FALSE, xlim=c(1e+06,7e+06), main = paste(toString(time), "subsamples with 100 values each"))
  avg = mean(means)
  SD = sd(means)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  lines(density(means, na.rm=TRUE), col="red")
}

#vector of numbers of values for 1280 subsamples
vals = c(20, 40, 80, 160, 320, 640, 1280, 2560)

for(value in vals){
  means= meansVector(1280, value, money14, "salary")
  hist(means, freq=FALSE, xlim=c(1e+06,7e+06), main = paste("1280 subsamples with", toString(value), "values each"))
  avg = mean(means)
  SD = sd(means)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  lines(density(means, na.rm=TRUE), col="red")
}

#####EXERCISE 2

#2.1: load data
data("mtcars")

#2.2: plot histogram of "mpg" variable
hist(mtcars$mpg, main = "distribution of mpg", breaks = 10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#2.3: series of histograms with increasing subsamples, fixed values
#code based on histograms in exercise 1

#vector of numbers of subsamples for 100 values each
mtcar_vars = c(20, 40, 80, 160, 320, 640, 1280, 2560)

for(time in mtcar_vars){
  means= meansVector(time, 100, mtcars, "mpg")
  hist(means, freq=FALSE, main = paste(toString(time), "subsamples with 100 values each"))
  avg = mean(means)
  SD = sd(means)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  lines(density(means, na.rm=TRUE), col="red")
}

#vector of numbers of values for 1280 subsamples
mtcar_vals = c(20, 40, 80, 160, 320, 640, 1280, 2560)

for(value in mtcar_vals){
  means= meansVector(1280, value, mtcars, "mpg")
  hist(means, freq=FALSE, main = paste("1280 subsamples with", toString(value), "values each"))
  avg = mean(means)
  SD = sd(means)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  lines(density(means, na.rm=TRUE), col="red")
}

#mean of means of subsamples is close to mean of mpg, demonstrating the central limit theorem