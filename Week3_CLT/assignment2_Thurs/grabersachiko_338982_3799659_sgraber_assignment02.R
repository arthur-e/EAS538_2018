#Assignment 2 19 Jan
setwd("~/Documents/MICHIGAN/538 stats/Lab 3")

install.packages("Lahman")
library(Lahman)
data("Salaries")

money14=subset(Salaries,yearID==2014)
money15=subset(Salaries,yearID==2015)

meansVector = function(times,size,dat,varb) {
  a=as.numeric(times)
  b=as.numeric(size)
  v=c()
  for(i in 1:a) {
    y=sample(dat[,varb],b,replace=TRUE)
    m=mean(y)
    v=c(v,m)
  }
  v
}

#####EXERCISE 1
hist(money14$salary, main="distribution of salary")
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg,col="blue")
legend("topright",legend=c(paste0("mean=",avg),paste0("SD",SD)),text.col=c("blue","dark green"))

#Take 10 subsamples of 100 values each
sample1=meansVector(10,100,money14,"salary")
hist(sample1,main="10 subsamples with 100 values each")
avg1=mean(sample1)
SD1=sd(sample1)
abline(v=avg1,col="blue")
legend("topright",legend=c(paste0("mean=",avg1),paste0("SD=",SD1)),text.col=c("blue","dark green"))

#20 subsamples of 100 values each
sample2=meansVector(20,100,money14,"salary")
hist(sample2,main="20 subsamples with 100 values each")
avg2=mean(sample2)
SD2=sd(sample2)
abline(v=avg2,col="blue")
legend("topleft",legend=c(paste0("mean=",avg2),paste0("SD=",SD2)),text.col=c("blue","dark green"),cex=0.7)

#40 subsamples of 100 values each
sample3=meansVector(40,100,money14,"salary")
hist(sample3,main="40 subsamples with 100 values each")
avg3=mean(sample3)
SD3=sd(sample3)
abline(v=avg3,col="blue")
legend("topleft",legend=c(paste0("mean=",avg3),paste0("SD=",SD3)),text.col=c("blue","dark green"),cex=0.7)


#80 subsamples with 100 values each
sample4=meansVector(80,100,money14,"salary")
hist(sample4,main="40 subsamples with 100 values each")
avg4=mean(sample4)
SD4=sd(sample4)
abline(v=avg4,col="blue")
legend("topleft",legend=c(paste0("mean=",avg4),paste0("SD=",SD4)),text.col=c("blue","dark green"),cex=0.7)

#160 subsamples with 100 values each
sample5=meansVector(160,100,money14,"salary")
hist(sample5,main="160 subsamples with 100 values each")
avg5=mean(sample5)
SD5=sd(sample5)
abline(v=avg5,col="blue")
legend("topleft",legend=c(paste0("mean=",avg5),paste0("SD=",SD5)),text.col=c("blue","dark green"),cex=0.7)

#320 subsamples with 100 values each
sample6=meansVector(320,100,money14,"salary")
hist(sample6,main="320 subsamples with 100 values each")
avg6=mean(sample6)
SD6=sd(sample6)
abline(v=avg6,col="blue")
legend("topleft",legend=c(paste0("mean=",avg6),paste0("SD=",SD6)),text.col=c("blue","dark green"),cex=0.7)

#640 subsamples with 100 values each
sample7=meansVector(640,100,money14,"salary")
hist(sample7,main="640 subsamples with 100 values each")
avg7=mean(sample7)
SD7=sd(sample7)
abline(v=avg7,col="blue")
legend("topleft",legend=c(paste0("mean=",avg7),paste0("SD=",SD7)),text.col=c("blue","dark green"),cex=0.7)

#1280 subsamples with 100 values each
sample8=meansVector(1280,100,money14,"salary")
hist(sample8,main="1280 subsamples with 100 values each")
avg8=mean(sample8)
SD8=sd(sample8)
abline(v=avg8,col="blue")
legend("topleft",legend=c(paste0("mean=",avg8),paste0("SD=",SD8)),text.col=c("blue","dark green"),cex=0.7)

#2560 subsamples with 100 values each
sample9=meansVector(2560,100,money14,"salary")
hist(sample9,main="2560 subsamples with 100 values each")
avg9=mean(sample9)
SD9=sd(sample9)
abline(v=avg9,col="blue")
legend("topleft",legend=c(paste0("mean=",avg9),paste0("SD=",SD9)),text.col=c("blue","dark green"),cex=0.7)

###The distribution in this series of histograms, while holding the number of values constant
#but icreasing the number of subsamples, become more and more normal. The mean converges
#towards the mean of the actual distribution.
#Check that means converge
means=c(avg,avg1,avg2,avg3,avg4,avg5,avg6,avg7,avg8,avg9)
means


###Fix the number of subsamples and increase the value taken in each subsample
#1280 subsamples with 10 values each
sample11=meansVector(1280,10,money14,"salary")
hist(sample11,main="1280 subsamples with 10 values each")
avg11=mean(sample11)
SD11=sd(sample11)
abline(v=avg11,col="blue")
legend("topright",legend=c(paste0("mean=",avg11),paste0("SD=",SD11)),text.col=c("blue","dark green"),cex=0.7)

#1280 subsamples with 20 values each
sample12=meansVector(1280,20,money14,"salary")
hist(sample12,main="1280 subsamples with 20 values each")
avg12=mean(sample12)
SD12=sd(sample12)
abline(v=avg12,col="blue")
legend("topright",legend=c(paste0("mean=",avg12),paste0("SD=",SD12)),text.col=c("blue","dark green"),cex=0.7)

#1280 subsamples with 40 values each
sample13=meansVector(1280,40,money14,"salary")
hist(sample13,main="1280 subsamples with 40 values each")
avg13=mean(sample13)
SD13=sd(sample13)
abline(v=avg13,col="blue")
legend("topright",legend=c(paste0("mean=",avg13),paste0("SD=",SD13)),text.col=c("blue","dark green"),cex=0.7)

#1280 subsamples with 80 values each
sample14=meansVector(1280,80,money14,"salary")
hist(sample14,main="1280 subsamples with 80 values each")
avg14=mean(sample14)
SD14=sd(sample14)
abline(v=avg14,col="blue")
legend("topright",legend=c(paste0("mean=",avg14),paste0("SD=",SD14)),text.col=c("blue","dark green"),cex=0.7)

#1280 subsamples with 160 values each
sample15=meansVector(1280,160,money14,"salary")
hist(sample15,main="1280 subsamples with 160 values each")
avg15=mean(sample15)
SD15=sd(sample15)
abline(v=avg15,col="blue")
legend("topright",legend=c(paste0("mean=",avg15),paste0("SD=",SD15)),text.col=c("blue","dark green"),cex=0.7)

#1280 subsamples with 320 values each
sample16=meansVector(1280,320,money14,"salary")
hist(sample16,main="1280 subsamples with 320 values each")
avg16=mean(sample16)
SD16=sd(sample16)
abline(v=avg16,col="blue")
legend("topright",legend=c(paste0("mean=",avg16),paste0("SD=",SD16)),text.col=c("blue","dark green"),cex=0.7)

#1280 subsamples with 640 values each
sample17=meansVector(1280,640,money14,"salary")
hist(sample17,main="1280 subsamples with 640 values each")
avg17=mean(sample17)
SD17=sd(sample17)
abline(v=avg17,col="blue")
legend("topright",legend=c(paste0("mean=",avg17),paste0("SD=",SD17)),text.col=c("blue","dark green"),cex=0.7)

#1280 subsamples with 1280 values each
sample18=meansVector(1280,1280,money14,"salary")
hist(sample18,main="1280 subsamples with 1280 values each")
avg18=mean(sample18)
SD18=sd(sample18)
abline(v=avg18,col="blue")
legend("topright",legend=c(paste0("mean=",avg18),paste0("SD=",SD18)),text.col=c("blue","dark green"),cex=0.7)

#1280 subsamples with 2560 values each
sample19=meansVector(1280,2560,money14,"salary")
hist(sample19,main="1280 subsamples with 2560 values each")
avg19=mean(sample19)
SD19=sd(sample19)
abline(v=avg19,col="blue")
legend("topright",legend=c(paste0("mean=",avg19),paste0("SD=",SD19)),text.col=c("blue","dark green"),cex=0.7)

###The distribution continues to be normal, but increasing the sample size of each sample
#taken causes the distribution to become much more normal.

#####EXERCISE 2
data(mtcars)
hist(mtcars$mpg)
length(mtcars$mpg)

#fix sample size, increase number of subsamples
cars1=meansVector(2,15,mtcars,"mpg")
hist(cars1,main="2 subsamples with 15 values each")

cars2=meansVector(4,15,mtcars,"mpg")
hist(cars2,main="4 subsamples with 15 values each")

cars3=meansVector(8,15,mtcars,"mpg")
hist(cars3,main="8 subsamples with 15 values each")

cars4=meansVector(16,15,mtcars,"mpg")
hist(cars4,main="16 subsamples with 15 values each")

cars5=meansVector(32,15,mtcars,"mpg")
hist(cars5,main="32 subsamples with 15 values each")

#fix number of subsamples, increase sample size
cars6=meansVector(15,2,mtcars,"mpg")
hist(cars6,main="15 subsamples with 2 values each")

cars7=meansVector(15,4,mtcars,"mpg")
hist(cars7,main="15 subsamples with 4 values each")

cars8=meansVector(15,8,mtcars,"mpg")
hist(cars8,main="15 subsamples with 8 values each")

cars9=meansVector(15,16,mtcars,"mpg")
hist(cars9,main="15 subsamples with 16 values each")

cars10=meansVector(15,32,mtcars,"mpg")
hist(cars10,main="15 subsamples with 32 values each")