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


### Exercise 1
money14 = subset(Salaries, yearID==2014)
head(money14)
hist(money14$salary)

x = c(10,20,40,80,160,320,640,1280,2560)
#gradually increasing numbers of subsamples but fixed values taken in each subsample
for (val in x) {
  means = meansVector(val, 100, money14, "salary")
  hist(means)
}
#fixed numbers of subsamples but gradually increasing values taken in each subsample
for (val in x) {
  means = meansVector(1280, val, money14, "salary")
  hist(means)
}

### Exercise 2

data(mtcars)
hist(mtcars$mpg)

x = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)
#gradually increasing numbers of subsamples but fixed values taken in each subsample
for (val in x) {
  means = meansVector(val, 10, mtcars, "mpg")
  hist(means)
}
#fixed numbers of subsamples but gradually increasing values taken in each subsample
for (val in x) {
  means = meansVector(15, val, mtcars, "mpg")
  hist(means)
}


#data(Batting)
#bat15 = subset(Batting, yearID==2015 & AB>200)
#bat15$avg = bat15$H/bat15$AB


#avg.se = sd(bat15$avg)/sqrt(length(bat15$avg))
#ci.lo = mean(bat15$avg) - qnorm(0.95)*avg.se

#set.seed(61)
#avg.m=c()
#for (i in 1:10000){
 # avg.m[i]=mean(sample(bat15$avg, length(bat15$avg), replace=TRUE))
#}
