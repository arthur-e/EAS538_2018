#> setwd("C:/Users/Owner/Desktop/U of M Masters Program/Winter 2017/NRE 538/Lab/Assignments/Assignment 3")
#> library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
t.test(sample, mu-"specified value")
guns.MI=subset(guns, state=="Michigan")

#Exercise 1

#null hypothesis: The rate of violent crimes in Michigan is significantly greater than 1000 incidents per 10^5 people
#alternative hypothesis:  The rate of violent crimes in Michigan is not significantly greater than 1000 incidents per 10^5 people.
guns.MI[, "violent"]
guns.MI = subset(guns, state=="Michigan")
t.test (guns.MI[,"violent"], mu=1000)
#One tail t-test
t.test(guns.MI[,"violent"], mu=1000, alternative="greater")
#One Sample t-test

data:  guns.MI[, "violent"]
t = -18.128, df = 22, p-value = 1
alternative hypothesis: true mean is greater than 1000
95 percent confidence interval:
  666.343     Inf
sample estimates:
  mean of x 
695.213 
#I can't reject the null hypothesis because the p-value is greater than .05, it is 1

#Two tail t-test
t.test (guns.MI[,"violent"], mu=1000)
#One Sample t-test

data:  guns.MI[, "violent"]
t = -18.128, df = 22, p-value = 1.028e-14
alternative hypothesis: true mean is not equal to 1000
95 percent confidence interval:
  660.3454 730.0807
sample estimates:
  mean of x 
#I reject the null hypothesis because the p-value is less than .05; the the rate of violent crimes is not greater than 1000 per 10^5 people.

#Exercise 2

#Null hypothesis:  The murder rate of Washington State is significantly different from the murder rate of Minnesota State.
#Alternate hypothesis: The murder rate of Washington State is not significantly different from the murder rate of Minnesota State.

t.test(guns.WAS[,"murder"], guns.Min[,"murder"], paired=False)
guns.WAS=subset(guns, state=="Washington")
guns.MIN=subset(guns, state=="Minnesota")
t.test(guns.WAS[,"murder"], guns.MIN[,"murder"], paired=FALSE)
mean(guns.WAS)
muX=4.8
muY=2.7
sdX=1
sdY=1
set.seed(32)
X=rnorm(50, mean=muX, sd=sdX)
Y=rnorm(50,mean=muY, sd=sdY)
hist(X,breaks=30,col=rgb(1,0,0,0.5),xlim=c(0,10),ylim=c(0,10),xlab="Value", main="histogram of sample X and Y")
hist(Y,breaks=30,col=rgb(0,1,0,0.5), add=TRUE)
box()
set.seed(8877)
dif.s=c()
for(i in 1:10000){dif.s[i]=mean(sample(X, 50, replace=TRUE)) - mean(sample(Y,50,replace=TRUE))}
dif.s.mean=mean(dif.s)
dif.s.sd=sd(dif.s)
CI95.lo=sort(dif.s)[10000*0.025]
CI95.hi=sort(dif.s)[10000*0.975]
hist(dif.s, probability=TRUE, main="distribution of differences")
lines(density(dif.s),col="red")
abline(v=c(CI95.lo, CI95.hi), col="blue", lty="dashed")
abline(v=0, col="dark green", lty="solid", lwed=3)
2*(length(which(dif.s>0))/length(dif.s))
[1] 2
t.test(X,Y, paired=FALSE)
# Welch Two Sample t-test

data:  X and Y
t = 13.488, df = 96.699, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  1.977597 2.660037
sample estimates:
  mean of x mean of y 
4.845097  2.526280 

# p-value < 2.2e-16 which is less than .05, we reject the null: The murder rate of Washington State is significantly different from Minnesota State.
