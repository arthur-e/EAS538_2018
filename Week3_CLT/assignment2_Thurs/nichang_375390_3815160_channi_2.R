#Legends remained in topright corner as topleft blocks the plot
#Exercise 1  Create same series of histograms with players¡¯ salaries in 2014
setwd("D:/UM/SNRE/Winter 2017/NRE 538 Statistics/Week 3 Lab")
install.packages("Lahman")
library(Lahman)
data("Salaries")
money14=subset(Salaries,yearID==2014)
head(money14)

hist(money14$salary,main="distribution of salary")
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg,col="BLUE")
legend("topright",legend=c(paste0("mean=",avg),paste0("SD=",SD)),
       text.col=c("blue","dark green"))

meansVector=function(times,size,dat,varb){
  a=as.numeric(times)
  b=as.numeric(size)
  v=c()
  for(i in 1:a){
    y=sample(dat[,varb],b,replace=TRUE)
    m=mean(y)
    v=c(v,m)
  }
  v
}

#This is the code that's supposed to plot all the histograms, but is there any way
# I can add a pause after each cycle then initiate the next one by pressing "Enter"?
#Right now I can only view them one after another by using the "previous plot" button...
#And is there a way to keep the axis the same? Right now it is changed according
#to the values produced by meanVectors...

#Changing number subsamples
for(k in 0:8){
  a=10*2^k
  hist(meansVector(a,100,money14,"salary"),
       main=c(paste0("subsample=",a),paste0("value=",100)),
       probability=TRUE)
  lines(density(meansVector(a,100,money14,"salary"),na.rm=TRUE),col="RED")
  AVGseries=mean(meansVector(a,100,money14,"salary"))
  SDSeries=sd(meansVector(a,100,money14,"salary"))
  legend("topright",legend=c(paste0("mean=",AVGseries),paste0("SD=",SDSeries)),
         text.col=c("blue","dark green"))
}
#Changing number of values in each subsample
for(h in 0:8){
  b=10*2^h
  hist(meansVector(1280,b,money14,"salary"),
       main=c(paste0("subsample=",1280),paste0("value=",b)),
       probability=TRUE)
  lines(density(meansVector(1280,b,money14,"salary"),na.rm=TRUE),col="RED")
  AVGseries1=mean(meansVector(1280,b,money14,"salary"))
  SDSeries1=sd(meansVector(1280,b,money14,"salary"))
  legend("topright",legend=c(paste0("mean=",AVGseries1),paste0("SD=",SDSeries1)),
         text.col=c("blue","dark green"))
}

#Exercise 2 Demonstrate central limit theorem with the 
#¡°mpg (miles/gallon)¡± variable in the mtcars data set
#There are only 32 values in total, so I changed some parameters a bit...
data(mtcars)
hist(mtcars$mpg,main="distribution of mpg",breaks=10)
avgmt=mean(mtcars$mpg)
sdmt=sd(mtcars$mpg)
abline(v=avgmt,col="blue")
legend("topright",legend=c(paste0("mean=",avgmt),paste0("SD=",sdmt)),
                           text.col=c("blue","dark green"))

#head(mtcars$mpg)
#str(mtcars) Used this to find how many values there are when coding

#Changing number subsamples
for(k in 0:8){
  a=10*2^k
  hist(meansVector(a,20,mtcars,"mpg"),
       main=c(paste0("subsample=",a),paste0("value=",20)),
       probability=TRUE)
  lines(density(meansVector(a,20,mtcars,"mpg"),na.rm=TRUE),col="RED")
  AVGseries=mean(meansVector(a,20,mtcars,"mpg"))
  SDSeries=sd(meansVector(a,20,mtcars,"mpg"))
  legend("topright",legend=c(paste0("mean=",AVGseries),paste0("SD=",SDSeries)),
         text.col=c("blue","dark green"))
}

#Changing number of values in each subsample
for(h in 1:7){
  b=4*h
  hist(meansVector(1280,b,mtcars,"mpg"),
       main=c(paste0("subsample=",1280),paste0("value=",b)),
       probability=TRUE)
  lines(density(meansVector(1280,b,mtcars,"mpg"),na.rm=TRUE),col="RED")
  AVGseries1=mean(meansVector(1280,b,mtcars,"mpg"))
  SDSeries1=sd(meansVector(1280,b,mtcars,"mpg"))
  legend("topright",legend=c(paste0("mean=",AVGseries1),paste0("SD=",SDSeries1)),
         text.col=c("blue","dark green"))
}
