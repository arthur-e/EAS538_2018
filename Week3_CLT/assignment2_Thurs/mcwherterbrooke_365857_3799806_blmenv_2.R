##Assignment 2##

## Exercise 1: Try to create same series of histograms with players' salaries in 2014.##
#5 histrograms
install.packages("Lahman")
library(Lahman)
data("Salaries")
money14=subset(Salaries, yearID==2014)
head(money14)

#Example: #meansvector( 100 times,50, money15, "salary")
#5 Histograms (20,100)(40,100)(1280,10)(1280,20)(2560,100)
  
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

#Hist 1 (20,100)
meansVector(20, 100, money14, "salary")
hist(meansVector(20, 100, money14, "salary"), main="distribution of salary")
avg=mean(meansVector(20, 100, money14, "salary"))
SD=sd(meansVector(20, 100, money14, "salary"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist2(40,100)
meansVector(40, 100, money14, "salary")
hist(meansVector(40, 100, money14, "salary"), main="distribution of salary")
avg=mean(meansVector(40, 100, money14, "salary"))
SD=sd(meansVector(40, 100, money14, "salary"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist3(1280,10)
meansVector(1280, 10, money14, "salary")
hist(meansVector(1280, 10, money14, "salary"), main="distribution of salary")
avg=mean(meansVector(1280, 10, money14, "salary"))
SD=sd(meansVector(1280, 10, money14, "salary"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist4(1280,20)
meansVector(1280, 20, money14, "salary")
hist(meansVector(1280, 20, money14, "salary"), main="distribution of salary")
avg=mean(meansVector(1280, 20, money14, "salary"))
SD=sd(meansVector(1280, 20, money14, "salary"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist5(2560,100)
meansVector(2560,100, money14, "salary")
hist(meansVector(2560,100, money14, "salary"), main="distribution of salary")
avg=mean(meansVector(2560,100, money14, "salary"))
SD=sd(meansVector(2560,100, money14, "salary"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))


#Exercise 2#

#Read Data
data("mtcars")
str(mtcars)
mtcars$mpg

#Plot Histogram
hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist1 (20,100) #Increasing subsamples
meansVector(20,100,mtcars,"mpg")
hist(meansVector(20,100,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(20,100,mtcars,"mpg"))
SD=sd(meansVector(20,100,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist2(30,100)
meansVector(30,100,mtcars,"mpg")
hist(meansVector(30,100,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(30,100,mtcars,"mpg"))
SD=sd(meansVector(30,100,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist3(40,100)
meansVector(40,100,mtcars,"mpg")
hist(meansVector(40,100,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(40,100,mtcars,"mpg"))
SD=sd(meansVector(40,100,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist4(50,100)
meansVector(50,100,mtcars,"mpg")
hist(meansVector(50,100,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(50,100,mtcars,"mpg"))
SD=sd(meansVector(50,100,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist5(60,100)
meansVector(60,100,mtcars,"mpg")
hist(meansVector(60,100,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(60,100,mtcars,"mpg"))
SD=sd(meansVector(60,100,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist6(70,100) #Increasing Values
meansVector(70,100,mtcars,"mpg")
hist(meansVector(70,100,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(70,100,mtcars,"mpg"))
SD=sd(meansVector(70,100,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist7(70,200)
meansVector(70,200,mtcars,"mpg")
hist(meansVector(70,200,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(70,200,mtcars,"mpg"))
SD=sd(meansVector(70,200,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist8(70,300)
meansVector(70,300,mtcars,"mpg")
hist(meansVector(70,300,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(70,300,mtcars,"mpg"))
SD=sd(meansVector(70,300,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist9(70,400)
meansVector(70,400,mtcars,"mpg")
hist(meansVector(70,400,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(70,400,mtcars,"mpg"))
SD=sd(meansVector(70,400,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

#Hist10(70,500)
meansVector(70,500,mtcars,"mpg")
hist(meansVector(70,500,mtcars,"mpg"), main="distribution of Mpg")
avg=mean(meansVector(70,500,mtcars,"mpg"))
SD=sd(meansVector(70,500,mtcars,"mpg"))
abline(v=avg, col="blue") #add a vertivle line of the mean
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
