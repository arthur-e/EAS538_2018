### Assignment 3
### Christopher Askew-Merwin
### 1/30/2017

### Exercise 1 One-Tail or Two-Tail
# Compose a one-tail or a two-tail null hypotheses on another statistics of Michigan state and perform t-test to test the two hypotheses. For example, if the robbery rate is significantly greater than 300 per 10^5 people in Michigan state.
# Make sure to include your null hypothesis alond with your code and comment on the output of your statistics results.

# Null Hypothesis: The violent crime rate in Michigan is significantly equal to or less than 400.

guns = read.table("guns_NRE538.csv", header=T, fill=T, sep=",")

guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"violent"], mu=400, alternative="greater")

#One Sample t-test

#data:  guns.MI[, "violent"]
#t = 17.559, df = 22, p-value = 9.93e-15
#alternative hypothesis: true mean is greater than 400
#95 percent confidence interval:
#  666.343     Inf
#sample estimates:
#  mean of x 
#695.213 

# We reject the null hypothesis since the p-value = 9.93e-15

### Exercise #2
# Calculate the p-value of rejecting the following null hypothesis:
# H0H0: The murder rate of Washington state and murder rate of Minnesota state are not different from each other.

guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")

dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.MIN$murder, 23, replace=TRUE)) - mean(sample(guns.WAS$murder, 23, replace=TRUE))
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

# p-value = 0 so we reject the null hypothesis
