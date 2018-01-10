setwd("C:/Users/Owner/Desktop/U of M Masters Program/Winter 2017/NRE 538/Lab/Assignments/Assignment 2")
install.packages ("Lahman")
library(Lahman)
data("Salaries")


money14=subset(Salaries,yearID==2014)
head(money14)
hist(money14$salary, main="distribution of salary")
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg,col="blue")
legend("topright",legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))

#Exercise 1

#n=10
#r=100
means=meansVector(10, 100, money14, "salary")
hist(means)

#n=20
#r=100
means=meansVector(20,100, money14, "salary")
hist(means)  

#n=40
#r=100
means=meansVector(40, 100, money14, "salary")
hist(means)

#n=160
#r=100
means=meansVector(160,100,money14, "salary")
hist(means)

#n=320
#r=100
means=meansVector(320,100,money14,"salary")
hist(means)

#n=640
#r=100
means=meansVector(640,100,money14,"salary")
hist(means)

#n=1280
#r=100
means=meansVector(1280,100,money14,"salary")
hist(means)

#n=2560
#r=100
means=meansVector(2560,100,money14,"salary")
hist(means)

#n=1280
#r=10
means=meansVector(1280,10,money14,"salary")
hist(means)

#n=1280
#r=20
means=meansVector(1280,20,money14,"salary")
hist(means)

#n=1280
#r=40
means=meansVector(1280,40,money14,"salary")
hist(means)

#n=1280
#r=80
means=meansVector(1280,80,money14,"salary")
hist(means)

#n=1280
#r=160
means=meansVector(1280,160,money14,"salary")
hist(means)

#n=1280
#r=320
means=meansVector(1280,320,money14,"salary")
hist(means)

#n=1280
#r=640
means=meansVector(1280,640,money14,"salary")
hist(means)

#n=1280
#r=1280
means=meansVector(1280,1280,money14,"salary")
hist(means)

#n=1280
#r=2560
means=meansVector(1280,2560,money14,"salary")
hist(means)

#Exercise 2

data("mtcars")
head(mtcars)

hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=mtcars$mpg)
legend("topright", legend=c(paste0("mean=", avg), paste0("SD=",SD)), text.col=c("blue", "dark green"))

data("mtcars")
hist(mtcars$mpg,probability=TRUE)

mpg=subset(mtcars, var=mpg)
head(mpg)

#n=10
#r=50
mean=meansVector(10,50,mpg,"mtcars")
hist(mean)

#n=30
#r=50
mean=meansVector(30,50,mpg,"mtcars")
hist(mean)

#n=100
#r=50
mean=meansVector(100,50,mpg,"mtcars")
hist(mean)

#n=50
#r=20
mean=meansVector(50,20,mpg,"mtcars")
hist(mean)

#n=50
#r=100
mean=meansVector(50,100,mpg,"mtcars")
hist(mean)

#n=50
#r=150
mean=meansVector(50,150,mpg,"mtcars")
hist(mean)



#1. Model parameters the Confidence Interval is calculating for is population sample and sampling distribution of the mean

#2. The procedure used to calculate the CI is to use the mean of random resamples, calculate the standard deviation which is then divided by the square root of the sample size.
