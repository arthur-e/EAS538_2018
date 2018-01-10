library(Lahman)
data("Salaries")
money15=subset(Salaries, yearID=2015)
#Actually, I did remember how to do this, but forgot how to find the variables to use
head(money15)
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

hist(money15$salary, main = "distribution of salary")
avg=mean(money15$salary)
SD=sd(money15$salary)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
meansVector(10, 100, money15, "salary")

meansVector(40, 100, money15, "salary")

meansVector(80, 100, money15, "salary")

meansVector(160, 100, money15, "salary")

money14=subset(Salaries, yearID=2014)
head(money14)
meansVector(10, 100, money14, "salary")
hist(meansVector(10, 100, money14, "salary"))
meansVector(50, 100, money14, "salary")
hist(meansVector(50, 100, money14, "salary"))
meansVector(100, 100, money14, "salary")
hist(meansVector(100, 100, money14, "salary"))
meansVector(150, 100, money14, "salary")
hist(meansVector(150, 100, money14, "salary"))
meansVector(500, 100, money14, "salary")
hist(meansVector(500, 100, money14, "salary"))
# So, here I can see every time I make these histograms, the graph starts to look more anad more like a bell curve
# i.e as the sample size increases, the closer to normal distribution it gets
data("mtcars")
hist(mtcars$mpg)
hist(mtcars$mpg, main = "distribution of mpg", breaks=10)
avg=mean(mtcars$mpg)
SD=sd(mtcars$mpg)
abline(v=avg, col="blue")
legend("topright", legend = c(paste0("mean=", avg), paste0("SD=", SD)),text.col=c("blue", "dark green"))
meansVector(5, 100, mtcars, "mpg")
hist(meansVector(5, 100, mtcars, "mpg"))
meansVector(100, 100, mtcars, "mpg")
hist(meansVector(100, 100, mtcars, "mpg"))
meansVector(100, 150, mtcars, "mpg")
hist(meansVector(100, 150, mtcars, "mpg"))
meansVector(100, 200, mtcars, "mpg")
hist(meansVector(100, 200, mtcars, "mpg"))
meansVector(100, 300, mtcars, "mpg")
hist(meansVector(100, 300, mtcars, "mpg"))
meansVector(100, 500, mtcars, "mpg")
hist(meansVector(100, 500, mtcars, "mpg"))
meansVector(100, 1000, mtcars, "mpg")
hist(meansVector(100, 1000, mtcars, "mpg"))
# I was about to say the data is still skewed but with more samples, but once I got to a high enough number it looked like maybe it was trending towards normal? I'm not sure
# is adding more subsamples the same as adding more values taken? Does it have the same effect (it looks like it)
#Confidence intervals
library(Lahman)
data("Batting")
bat15= subset(Batting, yearID==2015 & AB>200)
bat15$avg= bat15$H/bat15$AB
hist(bat15$avg, breaks = 30, freq = FALSE)
lines(density(bat15$avg), col = "salmon")
#I just want to note here that R is recognizing salmon as a real color but not aqua
shapiro.test(bat15$avg)

qnorm(bat15$avg); qqline(bat15$avg, col = "magenta")
sd= sd(bat15$avg)
se= sd/sqrt(length(bat15$avg))
CI.lo= mean(bat15$avg) - se*qnorm(.975)
CI.hi= mean(bat15$avg) + se*qnorm(.975)
set.seed(61)
avg.m=c()
for(i in 1:10000){
  avg.m[i]= mean(sample(bat15$avg, length(bat15$avg), replace=TRUE))
}
mean.resamp=mean(avg.m)
mean.se.resamp= sd(avg.m)
mean.CI.resamp=c(sort(avg.m)[10000*.025], sort(avg.m)[10000*.975])
print(paste0("standard error calculated by resampling", round(mean.CI.resamp, 5)))
print(paste0("95% confidence interval by resampling", round(avg.m)[10000*.025],4), "-", round(sort(avg.m)[10000*.975],4)))
#There's an extra () in these last two somewhere... I can't find where
