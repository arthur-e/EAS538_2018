install.packages("RCurl")
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")

### Exercise 1

robbery.MI = subset(guns, state=="Michigan")

hist(robbery.MI[,"robbery"], breaks=20)
abline(v=mean(robbery.MI[,"robbery"]), col="blue")
abline(v=200, col="dark green")
legend("topleft", legend = c("sample mean", "specified value=300"),text.col=c("blue", "dark green"))

## one sample, two-tailed t-test
# Null hypothesis: "The robbery rate in Michigan state is equal to 300 per 10^5 people"
t.test(robbery.MI[,"robbery"],mu=200)
# Reject the null hypothesis because p-value<0.05. 

## one sample, one-tailed t-test
# Null Hypothesis: "The robbery rate in Michigan is less than or equal to 300 per 10^5 people"
t.test(robbery.MI[,"robbery"],mu=200,alternative = 'greater')
# Reject the null hypothesis because p-value>0.05. 

### Exercise 2

guns.WA = subset(guns, state=="Washington")
guns.MN = subset(guns, state=="Minnesota")

hist(guns.WA[,"murder"], breaks=15, col="light blue", xlim=c(0, 7), ylim=c(0,5), xlab="murder rate", main="")
abline(v=mean(guns.WA[,"murder"]), col="blue")
par(new=TRUE)
hist(guns.MN[,"murder"], breaks=15, col="light green", xlim=c(0, 7), ylim=c(0,5), xlab="", main="")
abline(v=mean(guns.MN[,"murder"]), col="dark green")
legend("topright", legend = c("Washington mean", "Minnesota mean"),text.col=c("blue", "dark green"))

## calculating p-value
# Null hypothesis: "The murder rate in Washington and the murder rate in Minnesota are not different from each other"
set.seed(9487)
dif.s=c()
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.WA[, "murder"], 23, replace=TRUE)) - mean(sample(guns.MN[, "murder"], 23, replace=TRUE))
}
dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]
CI95.lo
CI95.hi
# 95% CI between 1.765 and 2.426, which does not overlap 0
# Reject the null hypothesis
p = 2*(length(which(dif.s<0))/length(dif.s))
p
# Reject the null hypothesis because p-value<0.05
# p-value=0

# visualizing the distribution
hist(dif.s, probability=TRUE, main="distribution of differences")
lines(density(dif.s), col="red")
abline(v=c(CI95.lo, CI95.hi), col="blue", lty="dashed")
abline(v=0, col="dark green", lty="solid", lwd=3)

legend("topright", legend = c("95% confidence interval", 
                              "0"), 
       col=c("blue", "dark green"),
       text.col=c("blue", "dark green"),
       lty=c("dashed", "solid"), bty="n")

## verifying p-value calculation with two sample, two-tailed t-test
# Null hypothesis: "The murder rate in Washington and the murder rate in Minnesota are not different from each other"
t.test(guns.WA[,"murder"],guns.MN[,"murder"],paired=FALSE)
# Reject the null hypothesis because p-value<0.05.
# p-value=1.546e-15

