# Exerise 1 (one sample t-test)

library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")

# One-tail t-test
# Null hypothesis: True mean of the Michigan robbery rate is smaller than 150
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"robbery"], mu=150, alternative="greater")
hist(guns.MI[,"robbery"], breaks=20)
abline(v=mean(guns.MI[,"robbery"]), col="blue")
abline(v=150, col="dark green")
legend("topleft", legend = c("sample mean", "specified value=150"),text.col=c("blue", "dark green"))
# Since the P-value is much smaller than 0.05, so we reject the null hypothesis and the true mean is greater than the expected value.

# Two-tail t-test
# Null hypothesis: True mean of the Michigan robbery rate is no different from 200
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"robbery"], mu=230)
hist(guns.MI[,"robbery"], breaks=20)
abline(v=mean(guns.MI[,"robbery"]), col="blue")
abline(v=230, col="dark green")
legend("topleft", legend = c("sample mean", "specified value=230"),text.col=c("blue", "dark green"))
# Since the P-value is greater than 0.05, so we fail to reject the null hypothesis and there is no significant difference between the expected value and true mean

# Exercise 2 (two sample t-test)
# Two samples two-tail t-test
# Null hypothesis: The murder rate of Washington state and murder rate of Minnesota state are not different from each other.
guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")
guns.WAS.mur =(guns.WAS$murder)
guns.MIN.mur =(guns.MIN$murder)
hist(guns.WAS.mur,col=rgb(1,0,0,0.5),xlim=c(0,10),ylim = c(0,10), xlab="Value",main="histogram of guns.WAS and guns.MIN in murder rate")
hist(guns.MIN.mur,col=rgb(0,1,0,0.5),add=TRUE)

#resampling
dif.s=c()
for(i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS.mur,23,replace=TRUE))-mean(sample(guns.MIN.mur,23,replace=TRUE))
}
dif.s.mean=mean(dif.s)
dif.s.sd=sd(dif.s)
CI95.lo=sort(dif.s)[10000*0.025]
CI95.hi=sort(dif.s)[10000*0.975]

hist(dif.s,probability=TRUE,main="distribution of differences",xlim=c(0,3),ylim = c(0,3))
lines(density(dif.s),col="red")
abline(v=c(CI95.lo,CI95.hi),col="blue",lty="dashed")
abline(v=0,col="dark green",lty="solid",lwd=3)

legend("topright",legend=c("95% condifence interval","0"),col=c("blue", "dark green"), text.col=c("blue","dark green"),lty=c("dashed","solid"),bty="n")

2*(length(which(dif.s<0))/length(dif.s))

t.test(guns.WAS.mur,guns.MIN.mur,var.equal=TRUE,paired=FALSE)

# The p-value for rejecting the null hypothesis = 0 which means the Null hypothesis should be rejected. The murder rate of Washington state and Minnisota state are significantly different from each other.

