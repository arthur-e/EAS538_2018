install.packages("Lahman")
library(Lahman)
data("salaries")

#######Exercise 1 - player salary distribution 2014
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

###HISTOGRAMS OF SALARY DISTRIBUTION WITH NUMBER OF VALUES PER SUBSAMPLE HELD CONSTANT
#histogram of hypothetical salary distribution of the entire population
hist(money14$salary, main="distribution of salary")
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg,col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD",SD)),text.col = c("blue", "dark green"))

#histogram of salary distribution with 20 subsamples, 100 values each
sample1 = meansVector(20,100,money14,"salary")
hist(sample1, main="20 subsamples with 100 values each")

#80 subsamples, 100 values each
sample2 = meansVector(80,100,money14,"salary")
hist(sample2, main="80 subsamples with 100 values each")

#160 subsamples, 100 values 
sample3 = meansVector(160,100,money14,"salary")
hist(sample3, main="160 subsamples with 100 values each")

#640 subsamples, 100 values
sample4 = meansVector(640,100,money14,"salary")
hist(sample4, main="640 subsamples with 100 values each")

#2560 subsamples with 100 values each
sample5 = meansVector(2560,100,money14,"salary")
hist(sample5, main="2560 subsamples with 100 values each")

###HISTOGRAMS OF SALARY DISTRIBUTION WITH NUMBER OF SUBSAMPLES HELD CONSTANT
#1280 subsamples, 10 values each
sample6 = meansVector(1280,10,money14,"salary")
hist(sample6, main="1280 subsamples with 10 values each")

#1280 subsamples, 40 values each
sample7 = meansVector(1280,40,money14,"salary")
hist(sample7, main="1280 subsamples with 40 values each")

#1280 subsamples, 80 values each
sample8 = meansVector(1280,80,money14,"salary")
hist(sample8, main="1280 subsamples with 80 values each")

#1280 subsamples, 320 values
sample9 = meansVector(1280,320,money14,"salary")
hist(sample9, main="1280 subsamples with 320 values each")

#1280 subsamples, 1280 values 
sample10 = meansVector(1280,1280,money14,"salary")
hist(sample10, main="1280 subsamples with 1280 values each")



#######Exercise 2 - MPG distribution for mtcars
data("mtcars")
hist(mtcars$mpg)

hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

###HISTOGRAMS OF MPG WITH VALUES PER SUBSAMPLE HELD CONSTANT
#10 subsamples, 20 values each
sample11 = meansVector(10,20,mtcars,"mpg")
hist(sample11, main="10 subsamples with 20 values each")

#40 subsamples, 20 values each
sample12 = meansVector(40,20,mtcars,"mpg")
hist(sample12, main="40 subsamples with 20 values each")

#100 subsamples, 20 values each
sample13 = meansVector(100,20,mtcars,"mpg")
hist(sample13, main="100 subsamples with 20 values each")

#640 subsamples, 20 values each
sample14 = meansVector(640,20,mtcars,"mpg")
hist(sample14, main="640 subsamples with 20 values each")

#1280 subsamples, 20 values each
sample15 = meansVector(1280,20,mtcars,"mpg")
hist(sample15, main="1280 subsamples with 20 values each")

###HISTOGRAMS OF MPG WITH NUMBER OF SUBSAMPLES HELD CONSTANT
#640 subsamples, 5 values each
sample16 = meansVector(640,5,mtcars,"mpg")
hist(sample16, main="640 subsamples with 5 values each")

#640 subsamples, 10 values each
sample17 = meansVector(640,10,mtcars,"mpg")
hist(sample17, main="640 subsamples with 10 values each")

#640 subsamples, 15 values each
sample18 = meansVector(640,15,mtcars,"mpg")
hist(sample18, main="640 subsamples with 15 values each")

#640 subsamples, 25 values each
sample19 = meansVector(640,25,mtcars,"mpg")
hist(sample19, main="640 subsamples with 25 values each")

#640 subsamples, 30 values each
sample20 = meansVector(640,30,mtcars,"mpg")
hist(sample20, main="640 subsamples with 30 values each")