###Exercise 1
# baseline distribution of players' salaries
install.packages("Lahman")
library (Lahman)
data("Salaries")
money14=subset(Salaries,yearID==2014)
head(money14)
hist(money14$salary, main = "distribution of salary")
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

# histograms of players' salaries in 2014 applying different subsamples and values:

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

meansVector(10,50,money14,"salary")
hist1=meansVector(10,50,money14,"salary")
hist(hist1)
hist(hist1, probability=TRUE)
lines(density(hist1, na.rm=TRUE), col="red")

meansVector(40,100,money14,"salary")
hist2=meansVector(40,100,money14,"salary")
hist(hist2)
hist(hist2, probability=TRUE)
lines(density(hist2, na.rm=TRUE), col="red")

meansVector(160,100,money14,"salary")
hist3=meansVector(160,100,money14,"salary")
hist(hist3)
hist(hist3, probability=TRUE)
lines(density(hist3, na.rm=TRUE), col="red")

meansVector(1280,640,money14,"salary")
hist4=meansVector(1280,640,money14,"salary")
hist(hist4)
hist(hist4, probability=TRUE)
lines(density(hist4, na.rm=TRUE), col="red")

meansVector(1280,20,money14,"salary")
hist5=meansVector(1280,20,money14,"salary")
hist(hist5)
hist(hist5, probability=TRUE)
lines(density(hist5, na.rm=TRUE), col="red")

###Exercise #2

# reading in data and plotting histogram:
data("mtcars")
mtcars$mpg
hist(mtcars$mpg, main = "distribution of mpg")
hist(mtcars$mpg, probability=TRUE)
lines(density(mtcars$mpg, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

# 5 graphs of increasing number of subsamples, fixed values:

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

meansVector(10,50,mtcars,"mpg")
hist6=meansVector(10,50,mtcars,"mpg")
hist(hist6)
hist(hist6, probability=TRUE)
lines(density(hist6, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")

meansVector(50,50,mtcars,"mpg")
hist7=meansVector(50,50,mtcars,"mpg")
hist(hist7)
hist(hist7, probability=TRUE)
lines(density(hist7, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")

meansVector(100,50,mtcars,"mpg")
hist8=meansVector(100,50,mtcars,"mpg")
hist(hist8)
hist(hist8, probability=TRUE)
lines(density(hist8, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")

meansVector(500,50,mtcars,"mpg")
hist9=meansVector(500,50,mtcars,"mpg")
hist(hist9)
hist(hist9, probability=TRUE)
lines(density(hist9, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")

meansVector(1000,50,mtcars,"mpg")
hist10=meansVector(1000,50,mtcars,"mpg")
hist(hist10)
hist(hist10, probability=TRUE)
lines(density(hist10, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")

# 5 graphs with fixed number of subsamples, increasing number of values:

meansVector(50,10,mtcars,"mpg")
hist11=meansVector(50,10,mtcars,"mpg")
hist(hist11)
hist(hist11, probability=TRUE)
lines(density(hist11, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")

meansVector(50,100,mtcars,"mpg")
hist12=meansVector(50,100,mtcars,"mpg")
hist(hist12)
hist(hist12, probability=TRUE)
lines(density(hist12, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")

meansVector(50,250,mtcars,"mpg")
hist13=meansVector(50,250,mtcars,"mpg")
hist(hist13)
hist(hist13, probability=TRUE)
lines(density(hist13, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")

meansVector(50,500,mtcars,"mpg")
hist14=meansVector(50,500,mtcars,"mpg")
hist(hist14)
hist(hist14, probability=TRUE)
lines(density(hist14, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")

meansVector(50,1500,mtcars,"mpg")
hist15=meansVector(50,1500,mtcars,"mpg")
hist(hist15)
hist(hist15, probability=TRUE)
lines(density(hist15, na.rm=TRUE), col="red")
avg=mean(mtcars$mpg)
abline(v=avg, col="blue")
