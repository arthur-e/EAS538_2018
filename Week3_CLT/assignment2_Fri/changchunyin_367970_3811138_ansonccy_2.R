install.packages("Lahman")
library(Lahman)
data("Salaries")

#Exercise1 

money14 = subset(Salaries, yearID==2014)
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

hist(money14$salary, main = "distribution of salary")
avg=mean (money14$salary)
SD=sd(money14$salary)
abline(v=avg, col="blue")
legend("topright",legend=c(paste0("mean=",avg),paste0("SD=",SD),text.col=c("blue", "dark green")))

# 10 subsamples with 50 values each
sal14.1=meansVector(10,50,money14,"salary")

hist(sal14.1, main="10 subsamples with 50values each",freq=FALSE)
avg=mean(sal14.1)
SD=sd(sal14.1)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.1, na.rm = TRUE),col="red")

# 20 subsamples with 100 values each
sal14.2=meansVector(20,100,money14,"salary")

hist(sal14.2, main="20 subsamples with 100values each",freq=FALSE)
avg=mean(sal14.2)
SD=sd(sal14.2)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.2, na.rm = TRUE),col="red")

# 40 subsamples with 100 values each
sal14.3=meansVector(40,100,money14,"salary")

hist(sal14.3, main="40 subsamples with 100values each",freq=FALSE)
avg=mean(sal14.3)
SD=sd(sal14.3)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.3, na.rm = TRUE),col="red")

# 80 subsamples with 100 values each
sal14.4=meansVector(80,100,money14,"salary")

hist(sal14.4, main="80 subsamples with 100values each",freq=FALSE)
avg=mean(sal14.4)
SD=sd(sal14.4)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.4, na.rm = TRUE),col="red")

# 160 subsamples with 100 values each
sal14.5=meansVector(160,100,money14,"salary")

hist(sal14.5, main="160 subsamples with 100values each",freq=FALSE)
avg=mean(sal14.5)
SD=sd(sal14.5)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.5, na.rm = TRUE),col="red")

# 320 subsamples with 100 values each
sal14.6=meansVector(320,100,money14,"salary")

hist(sal14.6, main="320 subsamples with 100values each",freq=FALSE)
avg=mean(sal14.6)
SD=sd(sal14.6)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.6, na.rm = TRUE),col="red")

# 640 subsamples with 100 values each
sal14.7=meansVector(640,100,money14,"salary")

hist(sal14.7, main="640 subsamples with 100values each",freq=FALSE)
avg=mean(sal14.7)
SD=sd(sal14.7)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.7, na.rm = TRUE),col="red")

# 1280 subsamples with 100 values each
sal14.8=meansVector(1280,100,money14,"salary")

hist(sal14.8, main="1280 subsamples with 100values each",freq=FALSE)
avg=mean(sal14.8)
SD=sd(sal14.8)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.8, na.rm = TRUE),col="red")

# 2560 subsamples with 100 values each
sal14.9=meansVector(2560,100,money14,"salary")

hist(sal14.9, main="2560 subsamples with 100values each",freq=FALSE)
avg=mean(sal14.9)
SD=sd(sal14.9)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.9, na.rm = TRUE),col="red")

# 1280 subsamples with 10 values each
sal14.10=meansVector(1280,10,money14,"salary")

hist(sal14.10, main="1280 subsamples with 10values each",freq=FALSE, xlim =c(0, 10000000))
avg=mean(sal14.10)
SD=sd(sal14.10)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.10, na.rm = TRUE),col="red")

# 1280 subsamples with 20 values each
sal14.11=meansVector(1280,20,money14,"salary")

hist(sal14.11, main="1280 subsamples with 20values each",freq=FALSE, xlim =c(0, 10000000))
avg=mean(sal14.11)
SD=sd(sal14.11)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.11, na.rm = TRUE),col="red")

# 1280 subsamples with 40 values each
sal14.12=meansVector(1280,40,money14,"salary")

hist(sal14.12, main="1280 subsamples with 40values each",freq=FALSE, xlim =c(0, 10000000))
avg=mean(sal14.12)
SD=sd(sal14.12)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.12, na.rm = TRUE),col="red")

# 1280 subsamples with 80 values each
sal14.13=meansVector(1280,80,money14,"salary")

hist(sal14.13, main="1280 subsamples with 80values each",freq=FALSE, xlim =c(0, 10000000))
avg=mean(sal14.13)
SD=sd(sal14.13)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.13, na.rm = TRUE),col="red")

# 1280 subsamples with 160 values each
sal14.14=meansVector(1280,160,money14,"salary")

hist(sal14.14, main="1280 subsamples with 160values each",freq=FALSE, xlim =c(0, 10000000))
avg=mean(sal14.14)
SD=sd(sal14.14)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.14, na.rm = TRUE),col="red")

# 1280 subsamples with 320 values each
sal14.15=meansVector(1280,320,money14,"salary")

hist(sal14.15, main="1280 subsamples with 320values each",freq=FALSE, xlim =c(0, 10000000))
avg=mean(sal14.15)
SD=sd(sal14.15)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.15, na.rm = TRUE),col="red")

# 1280 subsamples with 640 values each
sal14.16=meansVector(1280,640,money14,"salary")

hist(sal14.16, main="1280 subsamples with 640values each",freq=FALSE, xlim =c(0, 10000000))
avg=mean(sal14.16)
SD=sd(sal14.16)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.16, na.rm = TRUE),col="red")

# 1280 subsamples with 1280 values each
sal14.17=meansVector(1280,1280,money14,"salary")

hist(sal14.17, main="1280 subsamples with 1280values each",freq=FALSE, xlim =c(0, 10000000))
avg=mean(sal14.17)
SD=sd(sal14.17)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.17, na.rm = TRUE),col="red")

# 1280 subsamples with 2560 values each
sal14.18=meansVector(1280,2560,money14,"salary")

hist(sal14.18,main="1280 subsamples with 2560values each",freq=FALSE, xlim =c(0, 10000000))
avg=mean(sal14.18)
SD=sd(sal14.18)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(sal14.18, na.rm = TRUE),col="red")

# Exercise 2

data("mtcars")
hist(mtcars$mpg)

hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
# It's a pretty flat distribution (uniform distribution)

# 10 subsamples with 50 values each
EX2.1=meansVector(10,50,mtcars,"mpg")

hist(EX2.1, main="10 subsamples with 50values each",freq=FALSE, ylim=c(0,0.6))
avg=mean(EX2.1)
SD=sd(EX2.1)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(EX2.1, na.rm = TRUE),col="red")

# 100 subsamples with 50 values each
EX2.2=meansVector(160,50,mtcars,"mpg")

hist(EX2.2, main="160 subsamples with 50values each",freq=FALSE, ylim=c(0,0.6))
avg=mean(EX2.2)
SD=sd(EX2.2)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(EX2.2, na.rm = TRUE),col="red")

# 640 subsamples with 50 values each
EX2.3=meansVector(640,50,mtcars,"mpg")

hist(EX2.3, main="640 subsamples with 50values each",freq=FALSE, ylim=c(0,0.6))
avg=mean(EX2.3)
SD=sd(EX2.3)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(EX2.3, na.rm = TRUE),col="red")

# 2560 subsamples with 50 values each
EX2.4=meansVector(2560,50,mtcars,"mpg")

hist(EX2.4, main="2560 subsamples with 50values each",freq=FALSE, ylim=c(0,0.6))
avg=mean(EX2.4)
SD=sd(EX2.4)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(EX2.4, na.rm = TRUE),col="red")

# 10 subsamples with 160 values each
EX2.5=meansVector(10,160,mtcars,"mpg")

hist(EX2.5, main="10 subsamples with 160values each",freq=FALSE, xlim=c(15,25), ylim=c(0,3))
avg=mean(EX2.5)
SD=sd(EX2.5)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(EX2.5, na.rm = TRUE),col="red")

# 10 subsamples with 640 values each
EX2.6=meansVector(10,640,mtcars,"mpg")

hist(EX2.6, main="10 subsamples with 640values each",freq=FALSE, xlim=c(15,25), ylim=c(0,3))
avg=mean(EX2.6)
SD=sd(EX2.6)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(EX2.6, na.rm = TRUE),col="red")


# 10 subsamples with 2560 values each
EX2.7=meansVector(10,2560,mtcars,"mpg")

hist(EX2.7, main="10 subsamples with 2560values each",freq=FALSE, xlim=c(15,25), ylim=c(0,3))
avg=mean(EX2.7)
SD=sd(EX2.7)
abline(v=avg,col="blue")
legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)), text.col=c("blue", "dark green"))
lines(density(EX2.7, na.rm = TRUE),col="red")