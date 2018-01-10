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

## two sample, two-tailed t-test
# Null hypothesis: "The murder rate in Washington and the murder rate in Minnesota are not different from each other"
t.test(guns.WA[,"murder"],guns.MN[,"murder"],paired=FALSE)
# Reject the null hypothesis because p-value<0.05.
# p-value=1.546e-15

