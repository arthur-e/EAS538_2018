###EX1 One sample t-test
#HO (one-tailed): The mean robbery rate in Michigan is not significantly greater than 100.
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"robbery"], mu=100)
#the p-value is very small (2.72e-12) therefore we can reject the null hypothesis and say that the mean robbery rate in Michigan is significantly greater than 100.

hist(guns.MI[,"robbery"], breaks=20)
abline(v=mean(guns.MI[,"robbery"]), col="blue")
abline(v=100, col="dark green")
legend("topleft", legend = c("sample mean", "specified value=100"),text.col=c("blue", "dark green"))
#This conclusion is confirmed in our histogram plot. 

###EX2 Calculate p-value (manually)
#HO: The muder rate of Washington state and the murder rate of Minnesota are not different from eachother.
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.WAS= subset(guns, state=="Washington", select=3)
guns.MIN = subset(guns, state=="Minnesota", select=3)
means_MIN = (guns.MIN[, 1])
means_WAS = (guns.WAS[, 1])
set.seed(9487)
dif.s=c()
for (i in 1:10000){
  dif.s[i]=mean(sample(means_MIN, 30, replace=TRUE)) - mean(sample(means_WAS, 30, replace=TRUE))
}
dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]
2*(length(which(dif.s>0))/length(dif.s))
t.test(means_MIN, means_WAS, paired = FALSE)
#The mean murder rate in Minnesota is significantly different from the mean murder rate in Washington state. We know this because both our manually calculated p-value and the 'canned' R p-value are essentially zero.Therefore the two sample distributions do not overlap.
