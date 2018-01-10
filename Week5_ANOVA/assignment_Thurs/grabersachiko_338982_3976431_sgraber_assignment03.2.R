#Assignment 3 30 Jan
setwd("~/Documents/MICHIGAN/538 stats/Lab 4")
library(Lahman)
library(RCurl)
guns=read.csv("guns_NRE538.csv",header=TRUE,sep=",",comment.char="#")
guns.MI=subset(guns, state=="Michigan")

###EXERCISE 1
#1-tail null hypothesis 
#is the violent crime rate in Michigan significantly greater than 600 per 100,000?
#H0 the violent crime rate is less than 600
#Ha the violent crime rate is greater than 600
t.test(guns.MI[,"violent"],mu=600,alternative="less") #set the alternative to be the opposite of the null
hist(guns.MI[,"violent"],breaks=20,main="Histogram of violent crime rate, MI",ylim=c(0,2.75))
abline(v=mean(guns.MI[,"violent"]),col="red")
abline(v=600,col="blue")
legend("topleft",legend=c("sample mean","specified value=600"),text.col=c("red","blue"),cex=0.8)
#the violent crime rate in Michigan IS significantly greater than 600 per 100,000.
#we reject the null hypothesis that the violent crime rate <= 600

#2-tail null hypothesis
#is the violent crime rate in Michigan significantly different than 650 per 100,000?
#H0 the violent crime rate is 650
#Ha the violent crime rate is not 650
t.test(guns.MI[,"violent"],mu=650,alternative="two.sided")
hist(guns.MI[,"violent"],breaks=20,main="Histogram of violent crime rate, MI",ylim=c(0,3))
abline(v=mean(guns.MI[,"violent"]),col="red")
abline(v=650,col="blue")
legend("topleft",legend=c("sample mean","specified value=650"),text.col=c("red","blue"),cex=0.8)
#the violent crime rate in Michigan IS significantly different than 650 per 100,000 people.
#the 95% confidence interval is from 660-730.
#we reject the null hypothesis that the violent crime rate = 650.


###EXERCISE 2: calculate p-value of rejecting null hypothesis:
#H0 the murder rate in WA is the same as in MN
#Ha murder rate in WA is significantly different than MN
#2-tailed two value unpaired t-test

###EDIT
guns.WAS=subset(guns,state=="Washington")
guns.MIN=subset(guns,state=="Minnesota")
dif.g = c()
for (i in 1:10000) {
  dif.g[i] = mean(sample(guns.WAS[,"murder"],nrow(guns.WAS),replace=TRUE)) - 
    mean(sample(guns.MIN[,"murder"],nrow(guns.MIN),replace=TRUE))
}
dif.g.mean = mean(dif.g)
dif.g.sd = sd(dif.g)
CI95.g.lo = sort(dif.g)[10000*0.025]
CI95.g.hi = sort(dif.g)[10000*0.975]

hist(dif.g,probability=TRUE,main="Distribution of differences",xlim=c(0,5),col="light blue",xlab="murder rate")
lines(density(dif.g),col="red")
abline(v=c(CI95.g.lo,CI95.g.hi),col="blue",lty=2) 
abline(v=0,col="dark green",lty="solid",lwd=3)

p.g=2*length(which(dif.g<0)/length(dif.g))

#We reject the null hypothesis. The murder rate in WA is significantly different than MN



###EXERCISE 3
#see how probability would change using normal vs. runif() uniform distribution
t_stat = function(x,y) {#generates value of t-stat
  n=length(x)
  m=length(y)
  #compute pooled standard error
  sp=sqrt(((n-1)*sd(x)^2 + (m-1)*sd(y)^2)/(n+m-2)*(n+m)/(n*m))
  #compute test statistic
  (mean(x)-mean(y))/sp
}
n = 10
m = 10
crit = c(qt(0.025, n+m-2), qt(0.975, n+m-2))

##NORMAL DISTRIBUTION
nsim = 10000 # number of repetitions for ONE Monte Carlo simulation
nrep = 1000 # number of reps of Monte Carlo sim. creates several estimates of p(type 1 error)
val = matrix(NA, ncol=1, nrow=nsim)
prob = matrix(NA, ncol=1, nrow=nrep)

# Monte Carlo Simulation
set.seed(1302)
for (i in 1:nrep) {
  for (j in 1:nsim) {
    x = rnorm(n, 0, 1)
    y = rnorm(m, 0,10)
    # values of t-test statistic
    val[j] = t_stat(x,y)
  }
  # probability of type 1 error
  prob[i] = length(val[val> crit[2] | val< crit[1]])/ length(val)
}
p = round(mean(prob),3)
se = round(sd(prob)/sqrt(nrep),7)
print(paste0("probability of making type I error = ", p));
print(paste0("Standard error of the probability of making type I error = ", se))


##UNIFORM DISTRIBUTION
set.seed(1302)
for (i in 1:nrep) {
  for (j in 1:nsim) {
    x = rnorm(n, 0, 1)
    y = rnorm(m, 0,10)
    # values of t-test statistic
    val[j] = t_stat(x,y)
  }
  # probability of type 1 error
  prob[i] = length(val[val> crit[2] | val< crit[1]])/ length(val)
}
p = round(mean(prob),3)
se = round(sd(prob)/sqrt(nrep),7)
print(paste0("probability of making type I error = ", p)); #0.064
print(paste0("Standard error of the probability of making type I error = ", se)) #7.5E-5