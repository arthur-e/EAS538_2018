install.packages("Lahman")
library(Lahman)
data("Salaries")


#Exercise 1 create a histogram wih the players salaries from 2014

salaries14=subset(Salaries, yearID==2014)
head(salaries14)

hist(salaries14$salary, main= "Distribution of 2014 Salaries", xlab= "Salaries")


# this code returns the means of each subsample as a vector. 
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

# Code that runs a loop making histograms showing what happens when the number of times a mean is calculated from a sample with a constant sample size
for (i in 0:8){
  times = i*500+100
  size = 300
  hist(meansVector(times,size,salaries14,"salary"), main=c(times, "subsamples with" , size, "values"))
  avg=mean(salaries14$salary)
  SD=sd(salaries14$salary)
  abline(v=avg, col="blue") #overlays a verticle (v) line using the mean salary (avg)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green")) 
}

#I also ran a loop that makes historgrams showing what happens when the number of subsamples stays the same, but the sampe size increases
for (i in 0:8){
  times=1000
  size= i*200+100
  hist(meansVector(times,size,salaries14, "salary"), main=c(times, "subsamples with" , size, "values"))
  avg=mean(salaries14$salary)
  SD=sd(salaries14$salary)
  abline(v=avg, col="blue") #overlays a verticle (v) line using the mean salary (avg)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green")) 
}


#Exercise 2
#reading in the mtcars dataset from R
data("mtcars")

#plot the histogram of the data

hist(mtcars$mpg)

hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

