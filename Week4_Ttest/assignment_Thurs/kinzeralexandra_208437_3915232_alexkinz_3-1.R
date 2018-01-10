library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")

##EXERCISE ONE
##Compose a one-tail and a two-tail null hypotheses on another statistic of Michigan state and perform t-test to test the two hypotheses

##TWO-TAIL HYPOTHESIS
#Ho: The robbery rate in Michigan is significantly different than the national average.
#Ha: The robbery rate in Michigan is not significantly different.

#find the national average robbery rate /10^5 people
robberyavg = mean(guns$robbery)

#subset Michigan
robbery.MI = subset(guns, state=="Michigan")

#run t-test
t.test(robbery.MI[,"robbery"], mu=robberyavg)

#RESULT
#One Sample, two-tailed t-test

#data:  robbery.MI[, "robbery"]
#t = 7.4125, df = 22, p-value = 2.045e-07
#alternative hypothesis: true mean is not equal to 161.8202
#95 percent confidence interval:
#  213.7522 254.1000
#sample estimates:
#  mean of x 
#233.9261 

#Plot histogram
hist(robbery.MI[,"robbery"], breaks=20)
abline(v=mean(robbery.MI[,"robbery"]), col="blue")
abline(v=robberyavg, col="dark green")
legend("topleft", legend = c("Michigan robbery mean", "national average"),text.col=c("blue", "dark green"))

#Because the p-value is less than .05, we can cannot reject the null hypothesis. The robbery rate in Michigan is significantly different than the national average.

##Exercise 2

##Calculate the p-value of rejecting the following null hypothesis: 
#H0: The murder rate of Washington state is equal to that of Minnesota.
#HA: The murder rate of Washington state is different than that of Minnesota.

##First method
##subset each state and calculate means
guns.WAS = subset(guns, state=="Washington")
murder.WAS= mean(guns.WAS$murder)
guns.MIN = subset(guns, state=="Minnesota")
murder.MIN= mean(guns.MIN$murder)

##difference between the means
numerator = mean(guns.WAS$murder)-mean(guns.MIN$murder)-0
inside = ((23-1)*sd(guns.WAS$murder)^2+(23-1)*sd(guns.MIN$murder)^2)/(23+23-2)
##sp
sp = sqrt(inside)
##tstat
tstat2 = numerator/(sp*sqrt(1/23 + 1/23))
pval = (1 - pt(tstat2,44))*2
##pval = 1.3344 e-15

##double-check with t-test
t.test(guns.WAS$murder,guns.MIN$murder,var.equal=TRUE,paired=FALSE)
##t = 12.104, df = 44, p-value = 1.347e-15

##because the p-value is below .05, we can reject the null hypothesis--there is a significant difference between the two means.


##RESAMPLING METHOD
murder.WAS
murder.MIN
sdA= 1
sdB= 1
WAS = rnorm(23, mean=murder.WAS, sd=sdA)
MIN = rnorm(23, mean=murder.MIN, sd=sdB)

set.seed(1)
dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(WAS, 23, replace=TRUE)) - mean(sample(MIN, 23, replace=TRUE))
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

pval_resample = 2*(length(which(dif.s>0))/length(dif.s))
pval_resample
##because 1 falls outside of the confidence interval, we can reject the null hypothesis. There is a significant difference between the two means.
