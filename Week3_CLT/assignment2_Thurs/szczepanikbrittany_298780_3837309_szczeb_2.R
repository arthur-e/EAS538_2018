install.packages("Lahman")
library(Lahman)
data("Salaries")
  
#Exercise 1
money14 = subset(Salaries, yearID==2014)
head(money14)

hist(money14$salary, main = "distribution of salary")
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg, col="blue")

hist(meansVector(20, 100, money14, "salary"))
hist(meansVector(40, 100, money14, "salary"))
hist(meansVector(80, 100, money14, "salary"))
hist(meansVector(160, 100, money14, "salary"))
hist(meansVector(320, 100, money14, "salary"))
hist(meansVector(640, 100, money14, "salary"))
hist(meansVector(1280, 100, money14, "salary"))
hist(meansVector(2560, 100, money14, "salary"))
hist(meansVector(1280, 10, money14, "salary"))
hist(meansVector(1280, 20, money14, "salary"))
hist(meansVector(1280, 40, money14, "salary"))
hist(meansVector(1280, 80, money14, "salary"))
hist(meansVector(1280, 160, money14, "salary"))
hist(meansVector(1280, 320, money14, "salary"))
hist(meansVector(1280, 640, money14, "salary"))
hist(meansVector(1280, 1280, money14, "salary"))
hist(meansVector(1280, 2560, money14, "salary"))

#end exercise


#Exercise 2
data("mtcars")
hist(mtcars$mpg)

hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col=c("blue", "dark green"))

  hist(meansVector(10, 50, mtcars, "mpg"))
  hist(meansVector(20, 100, mtcars, "mpg"))
  hist(meansVector(40, 100, mtcars, "mpg"))
  hist(meansVector(80, 100, mtcars, "mpg"))
  hist(meansVector(160, 100, mtcars, "mpg"))
  hist(meansVector(320, 100, mtcars, "mpg"))
  hist(meansVector(640, 100, mtcars, "mpg"))
  hist(meansVector(1280, 100, mtcars, "mpg"))
  hist(meansVector(2560, 100, mtcars, "mpg"))
  hist(meansVector(1280, 10, mtcars, "mpg"))
  hist(meansVector(1280, 20, mtcars, "mpg"))
  hist(meansVector(1280, 40, mtcars, "mpg"))
  hist(meansVector(1280, 80, mtcars, "mpg"))
  hist(meansVector(1280, 160, mtcars, "mpg"))
  hist(meansVector(1280, 320, mtcars, "mpg"))
  hist(meansVector(1280, 640, mtcars, "mpg"))
  hist(meansVector(1280, 1280, mtcars, "mpg"))
  hist(meansVector(1280, 2560, mtcars, "mpg"))
  
##end Exercise##
  