library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")

##Exercise 1
#One-tailed one-sample test
#Alternate hypothesis: the mean violent crime rate in Michigan is >= 500
#Null hypothesis: the violent crime rate is less than 500
t.test(guns.MI[,"violent"], mu=500, alternative="greater")
#The p-value is extremely tiny, indicating that we can safely reject the null hypothesis that the crime rate is less than 500

##Exercise 2

wa = subset(guns,state=='Washington')
mn = subset(guns,state=='Minnesota')
n1 = length(wa)
n2 = length(mn)

set.seed(9487)
dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(wa$murder, n1, replace=TRUE)) - mean(sample(mn$murder, n2, replace=TRUE))
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

#We get a P value of 2. This indicates that washington and minnesota have significantly different murder rates.
