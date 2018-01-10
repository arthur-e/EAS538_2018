#install package and get data
install.packages("Lahman")
library(Lahman)
data("Salaries")

###Exercise 1###

#subset only data from 2014
money14 = subset(Salaries, yearID==2014) ##NOTE: = is for assignment, == is for logic
#print that subset
print(money14)

##ASSIGNMENT: Try to create same series of histograms with players’ salaries in 2014.

#histogram of raw data
hist(money14$salary, main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#create the subsampling function  
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

#create histograms of the subsamples (at least 5)  

#20 subsamples with 100 samples each
#meansVector(20, 50, money14, "salary")

hist(meansVector(20, 50, money14, "salary"), main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#40 subsamples with 100 samples each
#meansVector(40, 100, money14, "salary")

hist(meansVector(40, 100, money14, "salary"), main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#80 subsamples with 100 samples each
#meansVector(80, 100, money14, "salary")

hist(meansVector(80, 100, money14, "salary"), main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#160 subsamples with 100 samples each
#meansVector(160, 100, money14, "salary")

hist(meansVector(160, 100, money14, "salary"), main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#1280 subsamples with 100 samples each
#meansVector(1280, 100, money14, "salary")

hist(meansVector(1280, 100, money14, "salary"), main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#2560 subsamples with 100 samples each
#meansVector(2560, 100, money14, "salary")

hist(meansVector(2560, 100, money14, "salary"), main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#What do you observe from these series of histograms? 
  ##As the number of subsamples increases, the distribution gets closer to normal.
#What would happen with the increase of the number of subsamples?
  ##As the number of samples in each subsample increases, the distribution gets closer to normal.
  
#2560 subsamples with 200 samples each
hist(meansVector(2560, 200, money14, "salary"), main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#2560 subsamples with 400 samples each
hist(meansVector(2560, 400, money14, "salary"), main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
 
#2560 subsamples with 600 samples each  
hist(meansVector(2560, 600, money14, "salary"), main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))


  
###Exercise 2###

#Now demonstrate central limit theorem with the “mpg (miles/gallon)” variable in the mtcars (Motor Trend Car Road Tests) data set. mtcars is another built-in data set in the base package of R. You can use the meansVector function I wrote for you to generate a vector of means.

#hint: Make sure that you are able to
##(1) read in (mtcars) data,
  data("mtcars")
##(2) plotting the histogram of “mpg” variable
hist(mtcars$mpg, main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  
##(3) create a series of histograms with gradually increasing numbers of subsamples but fixed values taken in each subsample

#100 subsamples, 100 samples in each
hist(meansVector(100, 100, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#200 subsamples, 100 samples in each  
hist(meansVector(200, 100, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#300 subsamples, 100 samples in each
hist(meansVector(300, 100, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  
#400 subsamples, 100 samples in each
hist(meansVector(400, 100, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  
#500 subsamples, 100 samples in each
hist(meansVector(500, 100, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  
##(4) create a series of histograms with fixed numbers of subsamples but gradually increasing values taken in each subsample
#100 subsamples, 100 samples in each
hist(meansVector(100, 100, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#100 subsamples, 200 samples in each    
hist(meansVector(100, 200, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  
#100 subsamples, 300 samples in each
hist(meansVector(100, 300, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#100 subsamples, 400 samples in each  
hist(meansVector(100, 400, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  
#100 subsamples, 500 samples in each
hist(meansVector(100, 500, mtcars, "mpg"), main = "miles per gallon")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg, col="blue")
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
  




