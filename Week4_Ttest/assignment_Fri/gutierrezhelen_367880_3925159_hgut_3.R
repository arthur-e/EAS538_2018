library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
setwd("C:/Users/helen/Documents/winter_2017/538_stats/lab/W4")

#EXERCISE 1
#One-tailed
# H0: The violent crime rate in California is greater than 1000 per 1000000 people
# H1: The violent crime rate in California is less than 1000 per 1000000 people

guns.CA = subset(guns, state=="California")
t.test(guns.CA[,"violent"], alternative=c("less"), mu=1000)

#t-test results:

#One Sample t-test

#data:  guns.CA[, "violent"]
#t = -4.3211, df = 22, p-value = 0.0001377
#alternative hypothesis: true mean is less than 1000
#   95 percent confidence interval:
#   -Inf 926.1698
#sample estimates:
#   mean of x 
#   877.4826 

#Interpretation of test results:
#H0 is rejected with a p-value of 0.00014
# the average violent crimes in CA per 1000000 people is 877.48
# The upper limit of the CI is 926.17, which tells us that the true mean lies below this limit and is less than 1000. 

#Two-tailed
# H0: The violent crime rate in California is equal to 1000 per 1000000 people
# H1: The violent crime rate in California is not 1000 per 1000000 people

guns.CA = subset(guns, state=="California")
t.test(guns.CA[,"violent"], alternative=c("two.sided"), mu=1000)

#t-test results:
# One Sample t-test

# data:  guns.CA[, "violent"]
# t = -4.3211, df = 22, p-value = 0.0002754
# alternative hypothesis: true mean is not equal to 1000
#  95 percent confidence interval:
#  818.6809 936.2844
# sample estimates:
#  mean of x 
#  877.4826

#Interpretation of test results:
#H0 is rejected with a p-value of 0.00028
# the average violent crimes in CA per 1000000 people is 877.48
# The CI is 818.68 - 936.28, which tells us that the true mean lies within this limit and is not equal to 1000. 

#EXERCISE 2

#Ex. 2: Calc p value of rejecting H0
#H0: "The murder rate of Washington state and murder rate of Minnesota state are not different from each other."

guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")

meanWAS=mean(guns.WAS$murder)
sdWAS=sd(guns.WAS$murder)

meanMIN=mean(guns.MIN$murder)
sdMIN=sd(guns.WAS$murder)

A = rnorm(23, mean=meanWAS, sd=sdWAS)
B = rnorm(23, mean=meanMIN, sd=sdMIN)
hist(A,breaks=23,col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,10), xlab="Value", main="histogram of sample A and B")
hist(B,breaks=23,col=rgb(0,1,0,0.5),add=TRUE)
box()

dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(A, 23, replace=TRUE)) - mean(sample(B, 23, replace=TRUE))
}
dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]

hist(dif.s, probability=TRUE, main="distribution of differences")
lines(density(dif.s), col="red")
abline(v=c(CI95.lo, CI95.hi), col="blue", lty="dashed")
abline(v=0, col="green", lty="solid", lwd=3)

legend("topright", legend = c("95% confidence interval", 
                              "0"), 
       col=c("blue", "dark green"),
       text.col=c("blue", "dark green"),
       lty=c("dashed", "solid"), bty="n")

p=2*(length(which(dif.s<0))/length(dif.s))
t.test(A, B, paired = FALSE)

#INTERPRETATION OF RESULTS
# The calculated value of p (0) approximates the p-value produced in the t-test. In addition, the calculated CI approximates the CI produced in the test.
# Because the p-value is less than 0.05, H0 can be rejected.