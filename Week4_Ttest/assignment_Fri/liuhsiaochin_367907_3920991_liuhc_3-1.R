setwd("D:/NRE538/Lab")
guns=read.csv("D:/NRE538/Lab/guns_NRE538.csv",sep=",",header=T,comment.char="#")
head(guns)
guns.MI=subset(guns,state=="Michigan")

# Exercise 1
## one-tail t-test
### Is the violent crime rate in Michigan state greater than the certain value, which I set 650 incidents per 10^5 people?
#### Null hypothesis: The mean of violent crime rate in Michigan state is less than or equal to 650, u<=650
#### Alternative hypothesis: The mean of violent crime rate in Michigan state is greater than 650, u>650
t.test(guns.MI[,"violent"],mu=650,alternative="greater")
# Output: p-value=0.0067 < 0.05, which means we can reject the null hypothesis => The mean of violent crime rate in Michigan state is significantly greater than 650
# Check: 95 percent confidence interval=666.343-Inf, where 650 is outside the low limit

## two-tail t-test
### Is the violent crime rate in Michigan state significantly differed from the certain value, which I set 650 incidents per 10^5 people?
#### Null hypothesis: The mean of violent crime rate in Michigan state isn't different from 650, u=650
#### Alternative hypothesis: The mean of violent crime rate in Michigan state is different from 650, u!=650
t.test(guns.MI[,"violent"],mu=650)
# Output: p-value=0.0134 < 0.05, which means we can reject the null hypothesis => The mean of violent crime rate in Michigan state is significantly different from 650
# Check: 95 percent confidence interval=660.3453-730.0807, where 650 isn't within the interval

hist(guns.MI[,"violent"],breaks=20)
abline(v=mean(guns.MI[,"violent"],col="blue"))
abline(v=650,col="dark green")

# Exercise 2
guns.WAS=subset(guns,state=="Washington")
guns.MIN=subset(guns,state=="Minnesota")
## Test whether the distributions of both the murder rate of Washington and that of Minnesota follow the normal distribution
shapiro.test(guns.WAS[,"murder"])
shapiro.test(guns.MIN[,"murder"])
## P-value are 0.1933 and 0.9083 > 0.05, which means they both follow normal distribution
## Check by Q-Q plot
qqnorm(guns.WAS[,"murder"]);qqline(guns.WAS[,"murder"],col="red")
qqnorm(guns.MIN[,"murder"]);qqline(guns.MIN[,"murder"],col="red")
## Since they are normal distribution, we can do statistical test based on central limit theorem

### Plot the histograms to take a look
hist(guns.WAS[,"murder"],breaks=15,col="light blue",xlim=c(0,7),ylim=c(0,5),xlab="murder rate",main="")
abline(v=mean(guns.WAS[,"murder"]),col="blue")
par(new=TRUE)
hist(guns.MIN[,"murder"],breaks=15,col="light green",xlim=c(0,7),ylim=c(0,5),xlab="",main="")
abline(v=mean(guns.MIN[,"murder"]),col="dark green")
legend("topright",legend=c("Washington mean","Minnesota mean"),text.col=c("blue","dark green"))

## Test whether they have equal variances
var.test(guns.WAS[,"murder"],guns.MIN[,"murder"])
## p-value=0.6626 > 0.05, which means they have equal variances

### Test 1: Test the null hypothesis based on central limit theorem using two-sample t-test with equal variances
WAS.mean=mean(guns.WAS[,"murder"])
WAS.sd=sd(guns.WAS[,"murder"])
WAS.n=length(guns.WAS[,"murder"])
MIN.mean=mean(guns.MIN[,"murder"])
MIN.sd=sd(guns.MIN[,"murder"])
MIN.n=length(guns.MIN[,"murder"])
dif=WAS.mean-MIN.mean
sp=sqrt(((WAS.n-1)*WAS.sd^2+(MIN.n-1)*MIN.sd^2)/(WAS.n+MIN.n-2))
se=sp*sqrt(1/WAS.n+1/MIN.n)
t=(0-dif)/se
tc=qt(0.975,df=44)
CI95.hi=dif+tc*se
CI95.lo=dif-tc*se
### Confidence interval=1.754-2.455, where 0 is outside of the interval (left side) => reject the null hypothesis
### Test statistic(t)= -12.10 << -2.015 (the low critical t of 95% CI)
### Calculate p-value
2*(length(which(dif<0))/length(dif))
### p-value=0 <<0.05, which means we can "reject" the null hypothesis

### Test 2: Test the null hypothesis by resampling the differences between the murder rate of the two states
dif.s=c()
for(i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS[,"murder"],23,replace=TRUE))-mean(sample(guns.MIN[,"murder"],23,replace=TRUE))
}
dif.s.mean=mean(dif.s)
dif.s.sd=sd(dif.s)
CI95.lo=sort(dif.s[10000*0.025])
CI95.hi=sort(dif.s[10000*0.975])
CI95.hi
CI95.lo

### Plot the histogram to see whether 0 is outside the confidence interval (reject the null hypothesis)
hist(dif.s,probability=TRUE,main="distribution of differences",xlim=c(-0.1,3))
lines(density(dif.s),col="red")
abline(v=c(CI95.lo,CI95.hi),col="blue",lty="dashed")
abline(v=0,col="dark green",lty="solid",lwd=3)
legend("topright",legend=c("95% confidence interval","0"),text.col=c("blue","dark green"),col=c("blue","dark green"),lty=c("dashed","solid"),bty="n")
### line of 0 locates far outside of confidence interval (left side) => reject the null hypothesis

#### Calculate p-value
2*(length(which(dif.s<0))/length(dif.s))
#### p-value=0 <<0.05, which means we can "reject" the null hypothesis

# Check by t.test function
## Two-sample, unpaired t-test
t.test(guns.WAS[,"murder"],guns.MIN[,"murder"],var.equal=TRUE,paired=FALSE)
t.test(guns.MIN[,"murder"],guns.WAS[,"murder"],var.equal=TRUE,paired=FALSE)
## p-value=1.347e-15 << 0.05, approximate =0, which equals to the p-value we calculated in test 1 and 2
## which means we can "reject" the null hypothesis that the murder rate of Washington state and the murder rate of Minnesota state are not different from each other
## In other words, we are 95% confident that the murder rate of Washington state and that of Minnesota stae are significantly different from each other
