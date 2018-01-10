setwd("C:/Users/andre/OneDrive/Documents/538 - Natural Resource Statistics/R Analysis")


guns=read.csv("guns.csv", sep=",", header=T)

head(guns)

guns.MI = subset(guns, state=="Michigan")


#The average crime rate in Michigan is not equal to the average crime rate in the United States in 2015.
#H0= The average violent crime rate in Michigan is not equal to the United States AVerage in 2015 of 372.6 violent crimes per 10^5 people.
#HA= The average violent crime rate in Michigan is equal to the United States average in 2015 of 372.6 violent crimes per 10^5 people.

t.test(guns.MI[,"violent"], mu=372.6)


# I can reject the null hypothesis (p-value = 3.161e-15), and say that the violent crime rate in Michigan is statistically significantly different than the national
# violent crime rate in the United States in 2015.

#Question 2 
# Calculate the p-value of rejecting the following null hypothesis:
#H0=The murder rate of Washington State is not statistically significantly different than the murder rate of Minnesota
#HA=The murder rate of washington State is statistically significantly different than the murder rate of Minnesota



guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")


set.seed(9487)
dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS$murder, 23, replace=TRUE)) - mean(sample(guns.MIN$murder, 23, replace=TRUE))
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

#The probabililty is 0% or 0 that the mean of murder rate in Minnesota is greater than the mean murder rate in Washington. 
#Therefore, we reject the null hypothesis and we can say that the murder rates are statsitically significantly different,
#with the mean in Minnesota being greater than the mean in Washington.


