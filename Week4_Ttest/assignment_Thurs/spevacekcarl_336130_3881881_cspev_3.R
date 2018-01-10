##Exercise 1
##Null Hypothesis: The average per capita income of people committing violent gun crimes in Michigan is not greater than the US Poverty Rate of $11,880/year)
##one tail one sample t test
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state == "Michigan")
t.test(guns.MI$income, alternative = "greater", mu = 11880)

##p-value = 2.389e-08; Therefore, we can reject the null hypothesis that he average per capita income of people committing violent gun crimes in Michigan is not greater than the US Poverty Rate of $11,880/year).
##Because we reject the null hypothesis, we can infer that people that commit gun crimes in Michigan have a greater income than the US poverty level.


##Excercise 2
guns.MIN = subset(guns, state == "Minnesota")
guns.WAS = subset(guns, state == "Washington")

#test if normal distributions and equal variances
shapiro.test(guns.MIN$murder)
shapiro.test(guns.WAS$murder)
var.test(guns.MIN$murder, guns.WAS$murder)



dif.s=c()
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.MIN$murder, 20, replace=TRUE)) - mean(sample(guns.WAS$murder, 20, replace=TRUE))
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


p.value = 2*(length(which(dif.s>0))/length(dif.s))

##The p-value of rejecting the null hypothesis that the murder rate of Washington state is not significnatly different from that of Minnesota state is equal to 0. This means we can reject the null hypothesis because the p-value is less than 0.05.
