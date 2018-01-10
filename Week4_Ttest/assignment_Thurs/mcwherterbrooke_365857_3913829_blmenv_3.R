library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")



#Exercise 1
#Compose a one-tail or a two-tail null hypotheses on another statistics of Michigan state and perform t-test to test the two hypotheses. 
#Make sure to include your null hypothesis along with your code and comment on the output of your statistics results.

#Hypothesis: The  prison population has experienced a positive growth since 1977 in Alabama
#Null: The prison population has had no growth, the population contains a mean values less than or equal to 0

# T.test - one tail
prisoners.MI=subset(guns,state=="Michigan")
t.test(prisoners.MI[,"prisoners"], mu=5, alternative = 'greater') 

#Results:
#t=10.811, p-value=1.434e-10

#P-value is less than 0.05, I can reject the null, the difference is significant. 

#Exercise 2
#Calculate the p-value of rejecting the following null hypothesis:
#H0H0: The murder rate of Washington state is not different from that of Minnesota state.
#resampling for 10,000 times value should be 0

murder.MN=subset(guns,state=="Minnesota", select = c(murder))
murder.WA=subset(guns,state=="Washington",select = c(murder))

#Manual P-value calculations

#Two sample comparison - see if the two means are the same
muA=murder.MN
muB=murder.WA
sdA=1
sdB=1
set.seed(100) 
A = rnorm(22, mean=muA, sd=sdA)
B = rnorm(10000, mean=muB, sd=sdB)
hist(A,breaks=30,col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,10), xlab="Value", main="histogram of sample A and B")
hist(B,breaks=30,col=rgb(0,1,0,0.5),add=TRUE)
box()

#Resampling
set.seed(100)
dif.s=c()
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
#p-value
2*(length(which(dif.s>0))/length(dif.s))
# p-value=0.004 

#T.test check
t.test(murder.MN, murder.WA, paired=FALSE)
#Results
#pdata:  murder.MN and murder.WA
#t = -12.104, df = 44, p-value = 1.347e-15
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -2.454717 -1.753979
#sample estimates:
# mean of x mean of y 
#2.669565  4.773913 
