
##  Exercise 1

prisoners = read.table("guns_NRE538.csv", header=T, fill=T, sep=",")
head(prisoners)

prisoners.MI = subset(prisoners, state=="Michigan")  
t.test(prisoners.MI[,"prisoners"], mu=200)

hist(prisoners.MI[,"prisoners"], breaks=20)
abline(v=mean(prisoners.MI[,"prisoners"]), col="blue")
abline(v=9, col="orange")
legend("topleft", legend = c("sample mean", "specified value=9"),text.col=c("blue", "orange"))

# We reject the null hypothesis that the true mean of incarceration rate per 10^5 is equal to 200



##  Exercise 2 :Calculate the p-value of rejecting the following null hypothesis: 
##  H0: The murder rate of Washington state is significnatly different from that of Minnesota state.

muA=mean(guns.WAS[,"murder"])
muB=mean(guns.MIN[,"murder"])
sdA=sd(guns.WAS[,"murder"])
sdB=sd(guns.MIN[,"murder"])

#Is this code needed??
A = rnorm (mean=muA, sd=sdA)
B = rnorm(mean=muB, sd=sdB)
hist(A,breaks=30,col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,10), xlab="Value", main="histogram of sample A and B")
hist(B,breaks=30,col=rgb(0,1,0,0.5),add=TRUE)
box()

# Using the resampling method (do sample ize need to be used?)

dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS[,"murder"], replace=TRUE)) - mean(sample(guns.MIN[,"murder"], replace=TRUE))
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

# Zero is not included in the CI, so...we can reject the null hypothesis?

2*(length(which(dif.s>0))/length(dif.s))

t.test(guns.WAS[,"murder"], guns.MIN[,"murder"], paired=FALSE)


##########

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

