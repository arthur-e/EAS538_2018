####EXERCISE 1

#US poverty rate = $11,880 per person per year
#null hypothesis: The mean per capita personal income of people committing gun crimes in Michigan is less than or equal to (not greater than) the 2016 US poverty rate of $11,880 per capita.
#alt hypothesis: The mean per capita persoal income of people committing gun crimes in Michigan is greater than the 2016 US poverty rate of $11,880 per capita.

#subset Michigan data
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"income"], alternative = "greater", mu=11880)

#Results: P-Value=2.389e-08
#So, we can reject the null hypothesis that the per capita income of people committing gun crimes in Michigan is less than or equal to the poverty rate.
#The mean is $14,182.21, which is higher than the poverty rate of $11,880 which we tested against, so this makes sense.
#The income of people committing gun crimes in Michigan is significantly greater than the poverty rate.

####EXERCISE 2
#Null hypothesis: The murder rate of Washington is not significantly different from that of Minnesota.
guns.MIN=subset(guns,state=="Minnesota")
guns.WAS=subset(guns,state=="Washington")

#test for normal distribution and equal variance
shapiro.test(guns.MIN$murder)
shapiro.test(guns.WAS$murder)
var.test(guns.MIN$murder,guns.WAS$murder)

dif.s=c()
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.MIN$murder,20,replace=TRUE)) - mean(sample(B, 30, replace=TRUE))
}

dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]

#histogram
hist(dif.s, probability=TRUE, main="distribution of differences")
lines(density(dif.s), col="red")
abline(v=c(CI95.lo, CI95.hi), col="blue", lty="dashed")
abline(v=0, col="dark green", lty="solid", lwd=3)

legend("topright", legend = c("95% confidence interval", 
                              "0"), 
       col=c("blue", "dark green"),
       text.col=c("blue", "dark green"),
       lty=c("dashed", "solid"), bty="n")

#calculate p-value
2*(length(which(dif.s>0))/length(dif.s))
#p-value = 0
#Because the p-value is less than 0.05, we can reject the null hypothesis that the murder rate of Washington is not significantly different from that of Minnesota.
#The murder rate of Washington is significantly different from that of Minnesota.

#t-test to confirm calculation of p-value
t.test(guns.WAS$murder, guns.MIN$murder, paired = FALSE)
#According to t-test, p-value=1.546e-15

