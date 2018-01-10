install.packages("Lahman")
library(Lahman)
data("Salaries")

money15 = subset(Salaries, yearID==2015)

hist(money15$salary, main = "distribution of salary")
avg=mean(money15$salary)
SD=sd(money15$salary)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))


###Exercise 1
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

hist(meansVector(10, 50, money15, "salary"))
hist(meansVector(20, 100, money15, "salary"))
hist(meansVector(40, 100, money15, "salary"))
hist(meansVector(80, 100, money15, "salary"))
hist(meansVector(160, 100, money15, "salary"))
hist(meansVector(320, 100, money15, "salary"))
hist(meansVector(640, 100, money15, "salary"))


###Exercise 2
data("mtcars")

hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

hist(meansVector(10, 20, mtcars, "mpg"))
hist(meansVector(20, 20, mtcars, "mpg"))
hist(meansVector(40, 20, mtcars, "mpg"))
hist(meansVector(80, 20, mtcars, "mpg"))
hist(meansVector(160, 20, mtcars, "mpg"))
hist(meansVector(320, 20, mtcars, "mpg"))
hist(meansVector(640, 20, mtcars, "mpg"))

hist(meansVector(50, 5, mtcars, "mpg"))
hist(meansVector(50, 10, mtcars, "mpg"))
hist(meansVector(50, 15, mtcars, "mpg"))
hist(meansVector(50, 20, mtcars, "mpg"))
hist(meansVector(50, 25, mtcars, "mpg"))