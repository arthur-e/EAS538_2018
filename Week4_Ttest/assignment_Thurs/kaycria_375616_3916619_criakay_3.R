install.packages("RCurl")
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")

##Excersize One##
##One-Tailed T-test##
##Null Hypothesis: The incarceration rate in Michigan is greater than 190 per 10^5 people##
guns.MI = subset(guns, state="Michigan")
t.test(guns.MI[, "prisoners"], mu=190, two.sided="greater")
##t-statistic: 7.0034, Degrees of Freedom = 1172, p-value = 4.201e-1##
##fail to reject the null hypothesis##
hist(guns.MI[,"prisoners"], breaks=20)
abline(v=mean(guns.MI[,"prisoners"]), col="blue")
abline(v=190, col="dark green")
legend("topleft", legend = c("sample mean", "specified value=9"), text.col= c("blue", "dark green"))

####################

##Exercise 2##
guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")

dif.s=c()
for(i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS$murder, 23, replace=TRUE))- mean(sample(guns.MIN$murder, 23, replace = TRUE))
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

##p-value is 2##
##Murder rate in Washington is 2.105973 units higher than in Minnesota##