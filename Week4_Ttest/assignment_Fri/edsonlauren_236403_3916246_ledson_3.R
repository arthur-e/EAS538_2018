##Lauren Edson NRE 538 Lab 3 
#T test 

guns=read.csv('C:/Users/Lauren/Documents/NRE 538/guns_NRE538.csv')
#guns.MI = subset(guns, state=="Michigan")
#t.test(guns.MI[,"murder"], mu=9)


###Exercise 1
# One-tail
#Ho: The true mean of the robbery rate is equal to or less than 220 per 10^5 people in Michigan 
#Ha: The true mean of the robbery rate is greater than 220 per 10^5 people in Michigan 

guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"robbery"], mu=220, alternative= 'greater' )

#Output: One Sample t-test
# data:  guns.MI[, "robbery"]
# t = 1.4316, df = 22, p-value = 0.08316
# alternative hypothesis: true mean is greater than 220
# 95 percent confidence interval:
#   217.2223      Inf
# sample estimates: mean of x 
# 233.9261  
#
# This means that I reject the null hypothesis.

###Exercise 2
#Calculate the p-value (two-tailed) of rejecting the following null hypothesis: 
#Ho:The murder rate of Washington state and murder rate of Minnesota state are not different from each other.
#Ha: The murder rate of Washington state and murder rate of Minnesota state are different from each other.
#DO NOT USE T.TEST

guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")
#t.test(guns.WAS[,"murder"], guns.MIN[,"murder"], paired=FALSE, var.equal=TRUE)

muWAS=mean(guns.WAS$murder)
muMIN=mean(guns.MIN$murder)
sdWAS=sd(guns.WAS$murder)
sdMIN=sd(guns.MIN$murder)
A = rnorm(30, mean=muWAS, sd=sdWAS)
B = rnorm(30, mean=muMIN, sd=sdMIN)
hist(A,breaks=30,col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,10), xlab="Value", main="histogram of sample A and B")
hist(B,breaks=30,col=rgb(0,1,0,0.5),add=TRUE)

dif.s=c() 
# creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(A, 30, replace=TRUE)) - mean(sample(B, 30, replace=TRUE))
}
dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]
# CI: 2.00857 to 2.53262

hist(dif.s, probability=TRUE, main="distribution of differences")
lines(density(dif.s), col="red")
abline(v=c(CI95.lo, CI95.hi), col="blue", lty="dashed")
abline(v=0, col="dark green", lty="solid", lwd=3)

legend("topright", legend = c("95% confidence interval", "0"), 
       col=c("blue", "dark green"),
       text.col=c("blue", "dark green"),
       lty=c("dashed", "solid"), bty="n")

twotailp= 2*(length(which(dif.s>0))/length(dif.s))
#  two-tailed p value is equal to 2
#  This is the result because the two data sets have different lengths. I would reject the null hypothesis; the murder rates are different from each other