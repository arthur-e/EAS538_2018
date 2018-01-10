
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")


####EXERCISE 1 
####Compose a one-tail and a two-tail null hypotheses on another statistics of Michigan state and perform t-test to test the two hypotheses. 
####For example, if the robbery rate is significantly greater than 300 per 10^5 people in Michigan state. Make sure to include your null hypothesis along
####with your code and comment on the output of your statistics results.

prison.MI=subset(guns, state=="Michigan")
##NULL Hypothesis, one-tailed: The imprisonment rate in Michigan is not significantly greater than 200 per 10^5 population.
t.test(prison.MI[,"prisoners"], mu=200, alternative = "greater")
##p-value: 0.00176, reject null hypothesis.

##NULL Hypothesis, two-tailed: The imprisonment rate in Michigan is not different from the mean imprisonment rate across the nation.
mean(guns$prisoners)
#National mean: 226.5797
t.test(prison.MI[,"prisoners"], mu=226.5797)
##p-value: 0.03554, reject null hypothesis.


####EXERCISE 2
####Calculate the p-value of rejecting the following null hypothesis: 
####H0: The murder rate of Washington state and murder rate of Minnesota state are not different from each other.
murder.WA=subset(guns, state=="Washington")
murder.MN=subset(guns, state=="Minnesota")

##Check if distributions are normal:
shapiro.test(murder.WA$murder) #p-value:0.1933, fail to reject null hypothesis, assume normal distb
shapiro.test(murder.MN$murder) #p-value:0.9083, fail to reject null hypothesis, assume normal distb

##Check if variance are equal:
var.test(murder.MN[,"murder"], murder.WA[,"murder"])
##p-value: 0.6626, fail to reject null hypothesis, assume variance equal

##Assumptions are met. Unpaired T-test:
t.test(murder.WA[,"murder"], murder.MN[,"murder"], paired = FALSE, var.equal = TRUE)
##CI: 1.754 - 2.455
##p-value: 1.347e-15


##OR

set.seed(20000)
dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(murder.MN$murder, 30, replace=TRUE)) - mean(sample(murder.WA$murder, 30, replace=TRUE))
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

##CI: -1.8 to -2.4

2*(length(which(dif.s>0))/length(dif.s))
##p-value = 0


####EXERCISE 3 
####Use the code above to see how the probability would change when comparing a normal distribution to a uniformrunif() distribution.
