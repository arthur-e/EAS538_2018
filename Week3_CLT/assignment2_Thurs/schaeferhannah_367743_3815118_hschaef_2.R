install.packages("Lahman")
library(Lahman)
data("Salaries")
money15= subset(Salaries, yearID==2015)
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



meansVector(10, 5, money15, "salary")
hist(meansVector(10, 5, money15, "salary"))
meansVector(40, 100, money15, "salary")
hist(meansVector(40, 100, money15, "salary"))
meansVector(160, 100, money15, "salary")
hist(meansVector(160,100, money15, "salary"))
meansVector(1280, 40, money15, "salary")
hist(meansVector(1280, 40, money15, "salary"))
meansVector(1280, 320, money15, "salary")
hist(meansVector(1280, 320, money15, "salary"))




data(mtcars)
str(mtcars)
mtcars$mpg
hist(mtcars$mpg)

meansVector(500, 10, mtcars, "mpg")
hist(meansVector(500, 10, mtcars, "mpg"))
meansVector(500, 100, mtcars, "mpg")
hist(meansVector(500, 100, mtcars, "mpg"))
meansVector(500, 300, mtcars, "mpg")
hist(meansVector(500, 300, mtcars, "mpg"))
meansVector(500, 500, mtcars, "mpg")
hist(meansVector(500, 500, mtcars, "mpg"))
meansVector(500, 1000, mtcars, "mpg")
hist(meansVector(500, 1000, mtcars, "mpg"))


meansVector(20, 500, mtcars, "mpg")
hist(meansVector(20, 500, mtcars, "mpg"))
meansVector(100, 500, mtcars, "mpg")
hist(meansVector(100, 500, mtcars, "mpg"))
meansVector(200, 500, mtcars, "mpg")
hist(meansVector(200, 500, mtcars, "mpg"))
meansVector(400, 500, mtcars, "mpg")
hist(meansVector(400, 500, mtcars, "mpg"))
meansVector(500, 500, mtcars, "mpg")
hist(meansVector(500, 500, mtcars, "mpg"))



library(Lahman)
data(Batting)
bat15 = subset(Batting, yearID==2015 & AB>200)
bat15$avg = bat15$H/bat15$AB
hist(bat15$avg, breaks=30, freq=FALSE)
lines(density(bat15$avg), col="red")
shapiro.test(bat15$avg)
qqnorm(bat15$avg); qqline(bat15$avg, col="Red")


