##Exercise 1
#H0: The mean incarceration rate in Michigan was greater than 200 people per 10^5 members of the population for the previous year.
#H.alt: The mean incarcertaion rate in Michigan was less than 200 people per 10^5 members of the population for the previous year.
guns = read.csv("guns_NRE538.csv",sep=",", header=T, comment.char="#")
guns.MI=subset(guns,state=="Michigan")
t.test(guns.MI$prisoners, alternative=c("less"),mu=200)
#p-value = .998, which is greater than .05, which means that we cannot reject the null hypothesis and accept that the mean incarceration rate in Michigan was greater than 200 people per 10^5 members of the population. 

## Exercise #2
#H0: The murder rate of Washington state and murder rate of Minnesota state are not different from each other.
guns.WAS=subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")

#Manually calculating the p-value
#Method #1
dif.s=c()
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.MIN$murder, length(guns.MIN$murder), replace=TRUE)) - mean(sample(guns.WAS$murder, length(guns.WAS$murder), replace=TRUE))
  }
2*(length(which(dif.s>0))/length(dif.s))
#output=0. Given the distribution of the differences between the means as shown in hist(dif.s), the chance to observe a difference in the means greater than -1.5 (i.e., 0) and less than -2.7 is very small. Therefore, we can assume that the means are not the same (since the distribution does not cover 0), and can reject the null hypothesis that there is no difference between the murder rate in these two states.

#Method #2
WAS.mean=mean(guns.WAS$murder, na.rm=TRUE)
MIN.mean=mean(guns.MIN$murder, na.rm=TRUE)
numerator = WAS.mean - MIN.mean-0
inside = ((22)*sd(guns.MIN$murder)^2+(22)*sd(guns.WAS$murder)^2)/(44)
sp = sqrt(inside)
tstat2 = numerator/(sp*sqrt(1/23 + 1/23))
pval = (1 - pt(tstat2,44))*2
pval #1.33e-15, ~0

t.test(guns.MIN$murder,guns.WAS$murder, paired=FALSE, var.equal = TRUE) #p-value = 1.35e-15
#Since the p-value<.05, we can reject the null hypothesis and assume that there is a difference in the means between murder rates in Washington and Minnesota.

##Notes 
#Confidence intervals
WAS.mean=mean(guns.WAS$murder, na.rm=TRUE)
WAS.mean.SE=sqrt(var(guns.WAS$murder)/length(guns.WAS$murder))
WAS.mean.CI=c(WAS.mean-qnorm(0.975)*WAS.mean.SE, WAS.mean+qnorm(0.975)*WAS.mean.SE)
WAS.mean.CI

MIN.mean=mean(guns.MIN$murder, na.rm=TRUE)
MIN.mean.SE=sqrt(var(guns.MIN$murder)/length(guns.MIN$murder))
MIN.mean.CI=c(MIN.mean-qnorm(0.975)*MIN.mean.SE, MIN.mean+qnorm(0.975)*MIN.mean.SE)
MIN.mean.CI
# WAS.mean.CI = 4.521920 - 5.025906 and MIN.mean.CI = 2.440215 - 2.898915
# These do not overlap, which means that we can reject the null hypothesis that there is no difference in the means.


#Histograms of samples
guns.WAS=subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")
t.test(guns.WAS[,"murder"], guns.MIN[,"murder"], paired=FALSE) #[,"murder"] is another way than saying $"murder"
hist(guns.WAS[,"murder"], breaks=15, col="light blue", xlim=c(0, 7), ylim=c(0,5), xlab="murder rate", main="")
abline(v=mean(guns.WAS[,"murder"]), col="blue")
par(new=TRUE)
hist(guns.MIN[,"murder"], breaks=15, col="light green", xlim=c(0, 7), ylim=c(0,5), xlab="", main="")
abline(v=mean(guns.MIN[,"murder"]), col="dark green")
legend("topright", legend = c("Washington mean", "Minnesota mean"),text.col=c("blue", "dark green"))

