#hohsieh_2
####Exercise 1####
install.packages("Lahman")
library(Lahman)
data("Salaries")

money14 = subset(Salaries, yearID==2014)
head(money14)

meanVector = function(times, size, dat, varb){ 
  a = as.numeric(times)
  b = as.numeric(size)
  v = c()
  
  for(i in 1:a){
    y = sample(dat[,varb], b, replace = TRUE)
    m = mean(y)
    v = c(v,m)
  }
  v
}

##increase subsample times####
v=c()
for(i in c(10,20,40,80,160,320,640,1280,2560)){
  y = meanVector(i, 100, money14, "salary")
  hist(y, main = c(i, "subsamples with 100 values each"), probability = T, xlim = c(2000000, 6000000))
  avg = mean(y)
  SD = sd(y)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col = c("blue", "dark green"))
  lines(density(y, na.rm = TRUE), col="red")
}


##change the value taken in each subsample#####
v=c()
for(i in c(10,20,40,80,160,320,640,1280,2560)){
  y = meanVector(1280, i, money14,"salary")
  hist(y, main = c("1280 subsamples with ", i, " values each"), probability = T, xlim = c(0,9000000))
  avg = mean(y)
  SD = sd(y)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col = c("blue", "dark green"))
  lines(density(y, na.rm = TRUE), col="red")
}



####Exercise 2####
##(1)
data("mtcars")

##(2)
hist(mtcars$mpg)
hist(mtcars$mpg, main = "distribution of mpg", breaks = 10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright",legend = c(paste0("mean=", avg), paste0("SD=",SD)), text.col=c("blue", "dark green")
)



##(3)increase the numbers of subsamples####
v=c()
for(i in c(10,20,40,80,160,320,640,1280,2560)){
  y = meanVector(i, 100, mtcars, "mpg")
  hist(y, main = c(i, "subsamples with 100 values each"), probability = T, xlim = c(15, 25))
  avg = mean(y)
  SD = sd(y)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col = c("blue", "dark green"))
  lines(density(y, na.rm = TRUE), col="red")
}



##(4)increase the values taken in each subsample####
v=c()
for(i in c(10,20,40,80,160,320,640,1280,2560)){
  y = meanVector(1280, i, mtcars, "mpg")
  hist(y, main = c("1280 subsamples with ", i, " values each"), probability = T, xlim = c(15, 25))
  avg = mean(y)
  SD = sd(y)
  legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)), text.col = c("blue", "dark green"))
  lines(density(y, na.rm = TRUE), col="red")
}
