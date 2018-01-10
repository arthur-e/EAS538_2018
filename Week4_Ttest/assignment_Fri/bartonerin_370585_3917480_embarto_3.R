#Erin Barton
#NRE358_Statistics
#1.30.2017

#Lab3_WriteUp

library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"),sep=",",header=T,comment.char="#")

#Exercise 1: Compose a one-tail and two-tail null hypotheses on another statistic of Michigan 
#state and perform a t-test to test the two hypotheses. For exmapl if the robbery rate is 
#significantly greater than 300 per 10^5 people in Michigan state. 
#Make sure to include you null hypothesis along with your code and comment of the output 
#of your statistics result

guns.MI=subset(guns, state=="Michigan")

#one-tail 
t.test(guns.MI$violent, mu=600, alternative="greater")

#One Sample t-test

#data:  guns.MI$violent
#t = 5.6631, df = 22, p-value = 5.377e-06
#alternative hypothesis: true mean is greater than 600
#95 percent confidence interval:
#  666.343     Inf
#sample estimates:
#  mean of x 
#695.213 

#null hypothesis: The sample mean of the violent crime rate in Michigan is not significantly greater than 600 violent crimes per 10e5 people. 
#alternative hypothesis: The sample mean of the violent crime rate in Michigan is significantly greater than 600 violent crimes per 10e5 people.

#Based on this t-test, I can reject my null hypothesis as p<.05. This means there is a 5% chance I am committing a type 1 error.

#Histogram
hist(guns.MI$violent,breaks=20)
abline(v=mean(guns.MI$violent),col="blue")
abline(v=600,col="dark green")
legend("topleft",legen=c("sample mean","specified value=9"),text.col=c("blue","dark green"))


#two-tail
t.test(guns.MI$violent, mu=600)

#One Sample t-test

#data:  guns.MI$violent
#t = 5.6631, df = 22, p-value = 1.075e-05
#alternative hypothesis: true mean is not equal to 600
#95 percent confidence interval:
#  660.3454 730.0807
#sample estimates:
#  mean of x 
#695.213 

#null hypothesis: The sample mean of the violent crime rate in Michigan is not significantly different than 600 violent crimes per 10e5 people. 
#alternative hypothesis: The sample mean of the violent crime rate in Michigan is significantly different (either less than or greater) than 600 violent crimes per 10e5 people.

#Based on this t-test, I can reject my null hypothesis as the p<.05. This means that there is only a 5% chance I am committing a type 1 error by rejecting the null hypothesis. 

#Histogram
hist(guns.MI$violent,breaks=20)
abline(v=mean(guns.MI$violent),col="blue")
abline(v=600,col="dark green")
legend("topleft",legen=c("sample mean","specified value=9"),text.col=c("blue","dark green"))



#Exercise 2: Calculate the p-value of rejecting the following null hypothesis:
#H0:"The murder rate of Washington state and murder rate of Minnesota state are not different from each other."

guns.WA <- subset(guns,state=="Washington")
guns.MN <- subset(guns, state=="Minnesota")

shapiro.test(guns.WA$murder)
#Shapiro-Wilk normality test
#data:  guns.WA$murder
#W = 0.94148, p-value = 0.1933
qqnorm(guns.WA$murder)
qqline(guns.WA$murder,col="red")
#The results of these normality tests show that the data for guns.WA$murder is pretty much normally distributed 

shapiro.test(guns.MN$murder)
#Shapiro-Wilk normality test
#data:  guns.MN$murder
#W = 0.98013, p-value = 0.9083
qqnorm(guns.MN$murder)
qqline(guns.MN$murder,col="red")
#The results of these normality tests show that the data for guns.MN$murder is pretty much normally distributed 


dif.s=c() 
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.WA$murder, 23, replace=TRUE)) - mean(sample(guns.MN$murder, 23, replace=TRUE))
}
dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]

hist(dif.s, probability=TRUE, main="distribution of differences between WA and MN", xlab="differences")
lines(density(dif.s), col="red")
abline(v=c(CI95.lo, CI95.hi), col="blue", lty="dashed")
abline(v=0, col="dark green", lty="solid", lwd=3)

legend("topright", legend = c("95% confidence interval", 
                              "0"), 
       col=c("blue", "dark green"),
       text.col=c("blue", "dark green"),
       lty=c("dashed", "solid"), bty="n")

#1-pvalue <- probability you'd see a value within a range that includes the specified value, in this case 0
2*(length(which(dif.s>0))/length(dif.s))
# returns 2

#p-value <- probablity you'd see a value outside a range that includes the specified value, in this case 0
2*(length(which(dif.s<0))/length(dif.s))
#returns 0

#Given this p-value, we can reject the null hypothesis. Since 1-pvalue is 2 (i.e. the mean value of the difference), and we subtract Minnesota mean from the Washington mean, we can also conclude that the murder rate of Washington state is on average higher than the murder rate of Minnesota by 2 murders per 10e5 people. 

#t-test test
t.test(guns.WA$murder,guns.MN$murder, paired=FALSE)
#The p-value this shows is 1.546e-15, which is very close to 0. This confirms our p-value. 