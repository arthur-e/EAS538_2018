# NRE538
# Lab 2
# adirkes

# setwd("//engin-labs.m.storage.umich.edu/adirkes/windat.V2/Documents/NRE538/NRE538_lab2")
install.packages("Lahman")
library(Lahman)
data("Salaries")

##
meansVector = function(times, size, dat, varb){
  a = as.numeric(times) # how many subsamples?
  b = as.numeric(size) # sample size
  v=c()
  for(i in 1:a){
    y = sample(dat[,varb],b,replace=TRUE)
    m = mean(y)
    v = c(v,m)
  }
  v
}
##

# Exercise 1
# Submit five histograms
#meansVector(10,50,money15,"salary");
money14 = subset(Salaries,yearID==2014)
samples = c(40,40,320,1280,1280);
sizes = c(10,100,100,10,2560)
for(i in 1:5){
  vect = meansVector(samples[i],sizes[i],money14,"salary");
  hist(vect,xlim = c(10^6,7*10^6),
       main=paste0("Figure ",i,": ",samples[i]," subsamples with ",sizes[i]," values each"),
       xlab="Mean Salary ($)",ylab="Frequency")
}

##
# Exercise 2
# Part 1
data(mtcars)

# Part 2
hist(mtcars$mpg, xlab="Fuel Economy (mpg)", ylab="Frequency",
     main = "Figure 6: Histogram of Fuel Economy")

# Part 3: fixed subsample size
# L = length(mtcars$mpg) # 32
# (mtcars$mpg) # 10.4
# max(mtcars$mpg) # 33.9
samples = c(10,30,50,100,500);
sizes = c(10,10,10,10,10)
for(i in 1:5){
  vect = meansVector(samples[i],sizes[i],mtcars,"mpg")
  f=i+6
  hist(vect,xlim = c(15,30),
       main=paste0("Figure ",f,": ",samples[i]," subsamples with ",sizes[i]," values each"),
       xlab="Fuel Economy (mpg)",ylab="Frequency")
}

# Part 4: fixed number of subsamples
samples = c(10,10,10,10,10);
sizes = c(10,50,150,300,1000)
for(i in 1:5){
  vect = meansVector(samples[i],sizes[i],mtcars,"mpg")
  f=i+11
  hist(vect,xlim = c(15,25),
       main=paste0("Figure ",f,": ",samples[i]," subsamples with ",sizes[i]," values each"),
       xlab="Fuel Economy (mpg)",ylab="Frequency")
}

##
