guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"murder"], mu=9)
hist(guns.MI[,"murder"], breaks=20)abline(v=mean(guns.MI[,"murder"]), col="blue")
abline(v=9, col="dark green")
legend("topleft", legend = c("sample mean", "specified value=9"),text.col=c("blue", "dark green"))
hist(guns.MI[,"murder"], breaks=20)


##Ho= The violent crime rate in Michigan will be 
##statistically significant from 500 incidents 
##per 10^5 people.
t.test(guns.MI[,"violent"], mu=10)

hist(guns.MI[,"violent"], breaks=20)
abline(v=mean(guns.MI[,"violent"]), col="blue")
abline(v=500, col="dark green")
legend("topleft", legend = c("sample mean", "specified value=500"),text.col=c("blue", "dark green"))

##The T value for this result was 40.755, and the
##p value was <2.2e-16
##According to these results, we can reject the null
##hypothesis because the violent crime rate in Michigan
##is statistically different from 500 incidents per
##10^5 people


## mean of x mean of y 
##  4.773913  2.669565
sd(guns.MIN[,"murder"])
##0.5611958
sd(guns.WAS[,"murder"])
##0.6166017

muA=4.773913
muB=2.669565
sdA=0.6166017
sdB=0.5611958
set.seed(32)
A = rnorm(30, mean=muA, sd=sdA)
B = rnorm(30, mean=muB, sd=sdB)
hist(A,breaks=30,col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,10), xlab="Value", main="histogram of sample A and B")
hist(B,breaks=30,col=rgb(0,1,0,0.5),add=TRUE)
box()
set.seed(9487)
dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
dif.s[i]=mean(sample(A, 30, replace=TRUE)) - mean(sample(B, 30, replace=TRUE))
}
dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]
hist(dif.s, probability=TRUE, main="distribution of differences")
lines(density(dif.s), col="red")
abline(v=c(CI95.lo, CI95.hi), col="blue", lty="dashed")
abline(v=0, col="dark green", lty="solid", lwd=3)
legend("topright", legend = c("95% confidence interval",
"0"),
col=c("blue", "dark green"),
text.col=c("blue", "dark green"),
lty=c("dashed", "solid"), bty="n")
2*(length(which(dif.s>0))/length(dif.s))

## pvalue= 2
## We fail to reject the null hypothesis.


