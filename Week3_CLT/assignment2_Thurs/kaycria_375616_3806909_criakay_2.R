install.packages("Lahman")
library(Lahman)
data("Salaries")

money14= subset(Salaries, yearID==2014)
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
  
####Excersize 1####
meansVector(10, 50, money14, "salary")
hist(meansVector(10, 50, money14, "salary"))
meansVector(80, 100, money14, "salary")
hist(meansVector(80,100, money14, "salary"))
meansVector(640, 100, money14, "salary")
hist(meansVector(640,100, money14, "salary"))
meansVector(2560, 100, money14, "salary")
hist(meansVector(2560, 100, money14, "salary"))
meansVector(1280, 20, money14, "salary")
hist(meansVector(1280, 20, money14, "salary"))
meansVector(1280, 320, money14, "salary")
hist(meansVector(1280, 320, money14, "salary"))  
##The dataset gets closer to a normal distribution as you increase the number of subsamples or values


##Excersize Two##
data(mtcars)
mpg = mtcars$mpg
hist(mpg, main = "distribition of mpg", breaks=10)

meansVector(20, 10, mtcars, "mpg")
hist(meansVector(20, 10, mtcars, "mpg"))
meansVector(50, 10, mtcars, "mpg")
hist(meansVector(50, 10, mtcars, "mpg"))
meansVector(80, 10, mtcars, "mpg")
hist(meansVector(80, 10, mtcars, "mpg"))
meansVector(100, 10, mtcars, "mpg")
hist(meansVector(100, 10, mtcars, "mpg"))
meansVector(150, 10, mtcars, "mpg")
hist(meansVector(150, 10, mtcars, "mpg"))

meansVector(100, 5, mtcars, "mpg")
hist(meansVector(100, 5, mtcars, "mpg"))
meansVector(100, 10, mtcars, "mpg")
hist(meansVector(100, 10, mtcars, "mpg"))
meansVector(100, 20, mtcars, "mpg")
hist(meansVector(100, 20, mtcars, "mpg"))
meansVector(100, 25, mtcars, "mpg")
hist(meansVector(100, 25, mtcars, "mpg"))
meansVector(100, 32, mtcars, "mpg")
hist(meansVector(100, 32, mtcars, "mpg"))