#NRE538-Statistics Lab02
#Erin Barton 

setwd("/Users/erinmbarton/Documents/R Work/Class Work/Statistics Class/Lab Work/Stats_Lab02")
install.packages("Lahman")
library(Lahman)
data("Salaries")

meansVector=function(times, size, dat, varb){
  a=as.numeric(times)
  b=as.numeric(size)
  v=c() #empty vector for the for loop below
  for(i in 1:a){ #a is the number you put in the times place inside the fucntion
    y=sample(dat[,varb],b,replace=TRUE)
    m=mean(y) #calculate the means of the sample, y
    v=c(v,m)
  }
  v
}

money14<-subset(Salaries, yearID==2014)
m14_ss1 <- meansVector(10,100,money14,"salary")
hist(m14_ss1, main="distribution of salary", probability=TRUE)
avg=mean(money14$salary)
SD=sd(money14$salary)
abline(v=avg,col="blue") # creates line of the mean
legend("topright",legend=c(paste0("mean=",avg),paste0("SD=",SD)),text.col=c("blue","dark green"))

#Exercise 1: 
#varying number of times you subsample while keeping size of subsample the same
vars=c(10,20,40,80,160,320,640,1280,2560)
for (i in vars){
  x=meansVector(i,100,money14,"salary")
  hist(x, main="distribution of salary", probability=TRUE, xlim=c(1*10^6, 7*10^6))
  lines(density(x),col="red")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg,col="blue") 
  legend("topright",legend=c(paste0("mean=",avg),paste0("SD=",SD)),text.col=c("blue","dark green"))
}

#varying size of subsample while keeping number of times sampled the same
vars=c(10,20,40,80,160,320,640,1280,2560)
for (j in vars){
  y=meansVector(1280,j,money14,"salary")
  hist(y, main="distribution of salary", probability=TRUE, xlim=c(1*10^6, 7*10^6))
  lines(density(y),col="red")
  avg=mean(money14$salary)
  SD=sd(money14$salary)
  abline(v=avg,col="blue") 
  legend("topright",legend=c(paste0("mean=",avg),paste0("SD=",SD)),text.col=c("blue","dark green"))
}


#Exercise 2: use mtcars data to repeat exercise 1
data(mtcars)

vars=c(10,20,40,80,160,320,640,1280,2560)
for (k in vars){
  z=meansVector(k,100,mtcars,"mpg")
  hist(z, main="distribution of mpg", probability=TRUE, xlim=c(15,25))
  lines(density(z),col="red")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg,col="blue")
  legend("topright", legend=c(paste0("mean=",avg),paste0("SD=",SD)),text.col=c("blue","dark green"))
}

vars=c(10,20,40,80,160,320,640,1280,2560)
for (l in vars){
  a=meansVector(1280,l,mtcars,"mpg")
  hist(a, main="distribution of mpg", probability=TRUE, xlim=c(15,25))
  lines(density(a),col="red")
  avg=mean(mtcars$mpg)
  SD=sd(mtcars$mpg)
  abline(v=avg,col="blue") 
  legend("topright",legend=c(paste0("mean=",avg),paste0("SD=",SD)),text.col=c("blue","dark green"))
}

