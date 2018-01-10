### Assignment 3

library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")

##### Exercise 1 

guns.MI = subset(guns, state=="Michigan")

# One Tailed One Sample Null Hypothesis: The average per capita personal income of people committing gun crimes in Michigan is greater than or equal to (not less) the 2016 US poverty guideline for a one person household ($11,880)

t.test(guns.MI[,"income"], alternative="less", mu=11880)
mean(guns.MI$income) # mean is $14,182.21 

# Results: p-value = 1. The p-value is above .05, therefore, we fail to reject the null hypothesis that the average per capita personal income of people committing gun crimes in Michigan is not less than the 2016 US poverty guideline for a one person household ($11,880).


##### Exercise 2 - Calculate the p-value of rejecting the following null hypothesis: H0H0: The murder rate of Washington state is not significantly different from that of Minnesota state.

guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")

shapiro.test(guns.WAS$murder) #p-value = .1933 (greater than .05) so the distribution is not not normal
shapiro.test(guns.MIN$murder) #p-value = .9083 (greater than .05) so the distribution is not not normal
var.test(guns.WAS$murder, guns.MIN$murder) #p-value = .6626 (greater than .05) so not not equal variances


dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.MIN$murder, 20, replace=TRUE)) - mean(sample(guns.WAS$murder, 20, replace=TRUE))
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

pvalue = 2*(length(which(dif.s>0))/length(dif.s)) # p-value = 0. The p-value is less than .05, therefore, we can reject the null hypothesis that the murder rate of Washington state is not significantly different from that of Minnesota state.

# Using T-Test to confirm my calcultaion:
t.test(guns.WAS$murder, guns.MIN$murder, paired=FALSE) #p-value = 1.546e-15 (very, very close to 0)
