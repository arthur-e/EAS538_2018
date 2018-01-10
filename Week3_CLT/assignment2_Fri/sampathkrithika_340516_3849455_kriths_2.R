### EXERCISE 1
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

money14= subset(Salaries, yearID==2014)
money14
head(money14)
hist(money14$salary, main= "distribution of salary- 2014")
avg<- mean(money14$salary)
SD= sd(money14$salary)
abline(v=avg, col= "blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col= c("red", "orange"))

hist(meansVector(10,50,money14,"salary"))
hist(meansVector(20,100,money14,"salary"))
hist(meansVector(40,100,money14,"salary"))
hist(meansVector(80,100,money14,"salary"))
hist(meansVector(160,100,money14,"salary"))
hist(meansVector(320,100,money14,"salary"))
hist(meansVector(640,100,money14,"salary"))
hist(meansVector(1280,100,money14,"salary"))
hist(meansVector(2560,100,money14,"salary"))


### EXERCISE 2
data("mtcars")
head(mtcars)
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

hist(meansVector(2,6,mtcars,"mpg"))
hist(meansVector(4,6,mtcars,"mpg"))
hist(meansVector(8,6,mtcars,"mpg"))
hist(meansVector(16,6,mtcars,"mpg"))
hist(meansVector(32,6,mtcars,"mpg"))
