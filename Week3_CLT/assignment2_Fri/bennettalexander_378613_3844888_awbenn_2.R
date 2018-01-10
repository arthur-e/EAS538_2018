## preparations

## the working directory is set
setwd("/docs/umich/coursework/538/labs/lab03")

## the meansVector function is created to generate subsamples
meansVector = function(times, size, dat, varb){
  a = as.numeric(times)
  b = as.numeric(size)
  v = c()
  for(i in 1:a){
    y = sample(dat[,varb],b,replace=TRUE)
    m = mean(y)
    v = c(v,m)}
  v
}

## begin exercise 1

## data are imported and the salary subset is assigned to a variable
install.packages("Lahman")
library(Lahman)
data("Salaries")
money14 = subset(Salaries,yearID==2014)

## the distribution of salaries is plotted as a histogram
hist(money14$salary, main = "distribution of salary")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg, col="blue")
  legend("topright",legend = c(paste0("mean=",avg),paste0("SD=",SD)),
         text.col=c("blue", "dark green"))
## the data are highly skewed

## meansVector is executed and used to plot a density histogram
mean.sample = meansVector(10,50,money14,"salary")
hist(mean.sample,freq = FALSE,xlim = c(1000000,7000000),xlab = "means",
     main = "10 subsamples with 50 values each")
  lines(density(mean.sample), col = "red")
  avg = mean(mean.sample)
  SD = sd(mean.sample)
  legend("topleft",legend = c(paste0("mean=",round(avg,digits=2)), 
         paste0("SD=",round(SD,digits=2))),text.col=c("blue", "dark green"))

## a for loop is utilized to gradually increase sample number with a fixed 
## sample size
subsamples = 0
samplesize = 100
for(subsamples in c(10,20,40,80,160,640,1280,2560)){
  mean.sample = meansVector(subsamples,samplesize,money14,"salary")
  hist(mean.sample,freq = FALSE,xlim = c(1000000,7000000),xlab = "means",
       main = c(paste0(subsamples," subsamples with ",samplesize," values each")))
  lines(density(mean.sample), col = "red")
  avg = mean(mean.sample)
  SD = sd(mean.sample)
  legend("topleft",legend = c(paste0("mean=",round(avg,digits=2)), 
         paste0("SD=",round(SD,digits=2))),text.col=c("blue", "dark green"))
}
## the sampling distribution becomes more normal as the number of samples
## increases

## a for loop is utilized to gradually increase sample size with a fixed 
## number of subsamples
subsamples = 1280
samplesize = 0
for(samplesize in c(10,20,40,80,160,640,1280,2560)){
  mean.sample = meansVector(subsamples,samplesize,money14,"salary")
  hist(mean.sample,freq = FALSE,xlim = c(1000000,7000000),xlab = "means",
       main = c(paste0(subsamples," subsamples with ",samplesize," values each")))
  lines(density(mean.sample), col = "red")
  avg = mean(mean.sample)
  SD = sd(mean.sample)
  legend("topleft",legend = c(paste0("mean=",round(avg,digits=2)), 
         paste0("SD=",round(SD,digits=2))),text.col=c("blue", "dark green"))
}
## the sampling distribution becomes more normal as the sample size increases

## begin exercise 2

## import mtcars data
data("mtcars")

## the distribution of the mpg variable is plotted as a histogram
hist(mtcars$mpg, main = "distribution of mpg")
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright",legend = c(paste0("mean=",avg),paste0("SD=",SD)),
       text.col=c("blue", "dark green"))
## the distribution is fairly flat

## meansVector is executed and used to plot a density histogram
mean.sample = meansVector(10,50,mtcars,"mpg")
hist(mean.sample,freq = FALSE,xlim = c(15,25),xlab = "means",
     main = "10 subsamples with 50 values each")
lines(density(mean.sample), col = "red")
avg = mean(mean.sample)
SD = sd(mean.sample)
legend("topleft",legend = c(paste0("mean=",round(avg,digits=2)), 
       paste0("SD=",round(SD,digits=2))),text.col=c("blue", "dark green"))

## a for loop is utilized to gradually increase sample number with a fixed 
## sample size
subsamples = 0
samplesize = 100
for(subsamples in c(10,20,40,80,160,640,1280,2560)){
  mean.sample = meansVector(subsamples,samplesize,mtcars,"mpg")
  hist(mean.sample,freq = FALSE,xlim = c(15,25),xlab = "means",
       main = c(paste0(subsamples," subsamples with ",samplesize," values each")))
  lines(density(mean.sample), col = "red")
  avg = mean(mean.sample)
  SD = sd(mean.sample)
  legend("topleft",legend = c(paste0("mean=",round(avg,digits=2)), 
         paste0("SD=",round(SD,digits=2))),text.col=c("blue", "dark green"))
}
## the sampling distribution becomes more normal as the number of samples
## increases

## a for loop is utilized to gradually increase sample size with a fixed 
## number of subsamples
subsamples = 1280
samplesize = 0
for(samplesize in c(10,20,40,80,160,640,1280,2560)){
  mean.sample = meansVector(subsamples,samplesize,mtcars,"mpg")
  hist(mean.sample,freq = FALSE,xlim = c(15,25),xlab = "means",
       main = c(paste0(subsamples," subsamples with ",samplesize," values each")))
  lines(density(mean.sample), col = "red")
  avg = mean(mean.sample)
  SD = sd(mean.sample)
  legend("topleft",legend = c(paste0("mean=",round(avg,digits=2)), 
         paste0("SD=",round(SD,digits=2))),text.col=c("blue", "dark green"))
}
## the sampling distribution becomes more normal as the sample size increases
