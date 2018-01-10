####### EXERCISE 1 

install.packages("Lahman")
library(Lahman)
data("Salaries")

money14 = subset(Salaries, yearID ==2014)
head(money14)

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

## HISTOGRAM 1

hist1 = meansVector (100, 50, money14, "salary")

hist(hist1, probability = TRUE, main = "100 subsamples with 50 values each")
lines(density(hist1), col="red")

## HISTOGRAM 2

hist2 = meansVector (20, 100, money14, "salary")

hist(hist2, probability = TRUE, main = "20 subsamples with 100 values each")
lines(density(hist2), col="red")


## HISTOGRAM 3

hist3 = meansVector (40, 320, money14, "salary")

hist(hist3, probability = TRUE, main = "40 subsamples with 320 values each")
lines(density(hist3), col="red")


## HISTOGRAM 4

hist4 = meansVector (1280, 10, money14, "salary")

hist(hist4, probability = TRUE, main = "1280 subsamples with 10 values each")
lines(density(hist4), col="red")

## HISTOGRAM 5

hist5 = meansVector (1280, 20, money14, "salary")

hist(hist5, probability = TRUE, main = "1280 subsamples with 20 values each")
lines(density(hist5), col="red")

##### EXERCISE 2

## 1. Read in (mtcars) data

data("mtcars")

## 2. Histogram of of mpg variable

hist(mtcars$mpg, main = "Distribution of Miles per Gallon (mpg)")
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))

## 3. Series of histograms with gradually increasing #'s of subsamples but fixed values taken in each subsample

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

# histogram 1 

cars1 = meansVector (10, 10, mtcars, "mpg")

hist(cars1, probability = TRUE, main = "10 subsamples with 10 values each")
lines(density(cars1), col="red")


# histogram 2

cars2 = meansVector (20, 10, mtcars, "mpg")

hist(cars2, probability = TRUE, main = "20 subsamples with 10 values each")
lines(density(cars2), col="red")

# histogram 3

cars3 = meansVector (30, 10, mtcars, "mpg")

hist(cars3, probability = TRUE, main = "30 subsamples with 10 values each")
lines(density(cars3), col="red")

# histogram 4

cars4 = meansVector (40, 10, mtcars, "mpg")

hist(cars4, probability = TRUE, main = "40 subsamples with 10 values each")
lines(density(cars4), col="red")

# histogram 5

cars5 = meansVector (50, 10, mtcars, "mpg")

hist(cars5, probability = TRUE, main = "50 subsamples with 10 values each")
lines(density(cars5), col="red")

## 4. Series of histograms with fixed numbers of subsamples but gradually increasing values taken in each subsample

# histogram 6

cars6 = meansVector (10, 5, mtcars, "mpg")

hist(cars6, probability = TRUE, main = "10 subsamples with 5 values each")
lines(density(cars6), col="red")

# histogram 7

cars7 = meansVector (10, 10, mtcars, "mpg")

hist(cars7, probability = TRUE, main = "10 subsamples with 10 values each")
lines(density(cars6), col="red")

# histogram 8

cars8 = meansVector (10, 15, mtcars, "mpg")

hist(cars8, probability = TRUE, main = "10 subsamples with 15 values each")
lines(density(cars8), col="red")

# histogram 9 

cars9 = meansVector (10, 20, mtcars, "mpg")

hist(cars9, probability = TRUE, main = "10 subsamples with 20 values each")
lines(density(cars9), col="red")

# histogram 10

cars10 = meansVector (10, 25, mtcars, "mpg")

hist(cars10, probability = TRUE, main = "10 subsamples with 25 values each")
lines(density(cars10), col="red")