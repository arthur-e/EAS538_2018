install.packages("Lahman")
library(Lahman)
data("Salaries")

meansVector = function(times, size, dat, varb){
  a = as.numeric(times) #how many subsamples
  b = as.numeric(size)  #how many values per subsample
  v = c()
  for(i in 1:a){
    y = sample(dat[,varb],b,replace=TRUE)
    m = mean(y)
    v = c(v,m)
  }
  v
}

 
 ### Exercise 1 ###
 money14 <- subset(Salaries, yearID == 2014)
 
  for(i in 0:2){
    for(j in 0:3){
      times = i*500+100
      size = j*500+100
      hist(meansVector(times,size,money14, "salary"), main = c(times, " subsamples with ", size, " values each"))
    }
  }
  #I made a loop to build 12 histograms for me to demonstrate how the number
  # of subsamples and values affect the convergance of the the means to a
  # normal distribution
 
  ### ###


### Exercise 2 ###
data(mtcars)
hist(mtcars$mpg, breaks = 10)

#vary the number of subsamples
for(i in 0:5){
  times = i*500+100
  size = 10
  hist(meansVector(times,size,mtcars, "mpg"), main = c(times, " subsamples with ", size, " values each"))
}

#vary the number of values per subsample
data(mtcars)
for(j in 1:6){
  times = 1000
  size = j*5
  hist(meansVector(times,size,mtcars, "mpg"), main = c(times, " subsamples with ", size, " values each"))
}
 ### ###


