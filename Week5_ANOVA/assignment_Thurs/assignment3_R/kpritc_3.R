## EXERCISE 1
guns=read.csv("/Users/katiepritchard/Downloads/guns_NRE538.csv", sep=",", header=T, comment.char="#")
guns.MI=subset(guns,state=="Michigan")

## One sample, two tail t-test (alpha = 0.05)
## Ho: The average annual incidence of robbery in MI is equal to 200
t.test(guns.MI[,"robbery"], mu=200)
hist(guns.MI[,"robbery"], breaks=20)
abline(v=mean(guns.MI[,"robbery"]), col="blue")
abline(v=200, col="dark green")
legend("topright", legend = c("sample mean", "specified value=200"),text.col=c("blue", "dark green"))
## p = 0.002085. Since p<0.05, we reject the null hypothesis.

## One sample, one tail t-test (alpha = 0.05)
## Ho: The average annual incidence of robbery in MI is less than or equal to the standard (200 per 10^5 people/yr)
t.test(guns.MI["robbery"], mu=200, alternative='greater')
## p = 0.001043. Since p<0.05, we reject the null hypothesis.

## EXERCISE 2
##Ho: The sample means of murders in Washington versus Minnesota do not differ from each other at the 5% significance level
##Ha: The sample means of murders in Washington versus Minnesota do differ from each other at the 5% significance level

guns.WAS=subset(guns,state=="Washington")
guns.MIN=subset(guns,state=="Minnesota")
muWAS= mean(guns.WAS[,"murder"])
muMIN= mean(guns.MIN[,"murder"])
sdWAS= sd(guns.WAS[, "murder"])
sdMIN= sd(guns.MIN[, "murder"])
set.seed(200)
WAS = rnorm(23, mean=muWAS, sd=sdWAS)
MIN = rnorm(23, mean=muMIN, sd=sdMIN)
hist(WAS,breaks=23,col=rgb(1,0,0,0.5), xlim=c(0,6), ylim=c(0,6), xlab="Value", main="Histogram of MIN and WAS Murder Rates")
hist(MIN, breaks=23, col=rgb(0,1,0,0.5), add=TRUE)
box()

set.seed(500)
dif.s=c()
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS$murder,23,replace=TRUE)) - 
mean(sample(guns.MIN$murder,23,replace=TRUE))
}
dif.s.mean=mean(dif.s)
dif.s.sd=sd(dif.s)
CI95.lo=sort(dif.s)[10000*0.025]
CI95.hi=sort(dif.s)[10000*0.975] 
#The 95% CI by resampling is: 1.77 to 2.43

hist(dif.s, probability=TRUE, xlim=c(0,3),main="Distribution of Differences")
lines(density(dif.s), col="red")
abline(v=c(CI95.lo, CI95.hi), col="blue", lty="dashed")
abline(v=0, col="dark green", lty="solid", lwd=3)

legend("topright",legend=c("95% confidence interval",
                           
                           "0"),
       col=c("blue", "dark green"),
       text.col=c("blue", "dark green"),
       lty=c("dashed", "solid"), bty="n")

##Since the 95% confidence interval does not overlap with 0, we reject the null hypothesis that the two sample means do not differ from each other at the 5% significance level

pvalue = 2*(length(which(dif.s>0))/length(dif.s))
pvalue
##P value = 2

## Since the p value falls outside of the 95% CI, we can reject the null hypothesis and conclude that the sample means do differ from each other at the 5% significance level 