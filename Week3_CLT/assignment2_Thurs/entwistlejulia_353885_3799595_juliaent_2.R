library(Lahman)
data("Salaries")
#Exercise 1
money14 = subset(Salaries, yearID==2014)
head(2014)
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
meansVector(10, 5, money14, "salary")
hist(meansVector(10, 5, money14, "salary"))
hist(meansVector(20, 100, money14, "salary"))
hist(meansVector(40, 100, money14, "salary"))
hist(meansVector(80, 100, money14, "salary"))
hist(meansVector(160, 100, money14, "salary"))
#Exercise 2
data(mtcars)
str(mtcars)
mtcars$mpg
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
meansVector(10, 5, mtcars, "mpg")
hist(meansVector(10, 5, mtcars, "mpg"))
hist(meansVector(20, 5, mtcars, "mpg"))
hist(meansVector(40, 5, mtcars, "mpg"))
hist(meansVector(80, 5, mtcars, "mpg"))
hist(meansVector(160, 5, mtcars, "mpg"))
hist(meansVector(10, 10, mtcars, "mpg"))
hist(meansVector(10, 20, mtcars, "mpg"))
hist(meansVector(10, 40, mtcars, "mpg"))
hist(meansVector(10, 80, mtcars, "mpg"))
hist(meansVector(10, 160, mtcars, "mpg"))