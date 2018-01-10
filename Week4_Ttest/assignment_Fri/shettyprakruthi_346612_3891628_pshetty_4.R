setwd("/Users/prakruthishetty/Downloads")
guns=read.table("guns_NRE538.csv", sep=",", header=T, comment.char="#")

guns.MI = subset(guns, state=="Michigan")
guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")


### Exercise 1

###One-tail hypothesis
###The robbery rate of Michigan state is significantly greater than 300 per 10^5 people in the state.abb
t.test(guns.MI[,"robbery"], mu=300, alternative = "greater")
#t = -6.7924, df = 22, p-value = 1
#alternative hypothesis: true mean is greater than 300
#95 percent confidence interval:217.2223      Inf
#mean of x 233.9261
#RESULT: Since p-value is 1 and the mean is within the CI, the null hypothesis is true.

###Two-tail null hypothesis
###The robbery rate of Michigan state is equal to 300 per 10^5 people in the state.abb
t.test(guns.MI[,"robbery"], mu=300)
#t = -6.7924, df = 22, p-value = 7.974e-07
#alternative hypothesis: true mean is not equal to 300
#95 percent confidence interval:213.7522 254.1000
#mean of x 233.9261 
#RESULT: Since p-value is 7.974e-07 and the mean is within the CI, the null hypothesis is true.


###Exercise 2
#H0: The murder rate of Washington state is significnatly different from that of Minnesota state.
muMIN=mean(guns.MIN$murder)
muWAS=mean(guns.WAS$murder)
sdMIN= sd(guns.MIN$murder)
sdWAS = sd(guns.WAS$murder)

set.seed(9487)
dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.MIN, 20, replace=TRUE)) - mean(sample(guns.WAS, 20, replace=TRUE))
}
dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]

2*(length(which(dif.s>0))/length(dif.s))
#Since the p-value is 0 and the CI interval doesn't overlap  0, the null hypothesis can be rejected.

#Reconfirming with the t.test() function
t.test(guns.WAS$murder, guns.MIN$murder, paired = FALSE)
#t = 12.104, df = 43.616, p-value = 1.546e-15
#95 percent confidence interval:1.753891 2.454804
# Thus we can reject the null hypothesis since 0 doesnt fall under the CI.
