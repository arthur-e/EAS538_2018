##Exercise 1
#Null hypothesis for two-tailed--> Michigan state's violence rate is equal to 650 incidents per 10^5 people
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"violent"], mu=650)
#the p-value is 0.0134, which is less than 0.05, so we can reject the null hypothesis with little probably of committing a type I error.

##Exercise 2
#Null hypothesis: the murder rate of Washington state and murder rate of Minnesota state are not different from each other
guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")
t.test(guns.WAS[,"murder"], guns.MIN[,"murder"], paired=FALSE)

hist(guns.WAS[,"murder"], breaks=15, col="light blue", xlim=c(0, 7), ylim=c(0,5), xlab="murder rate", main="")
abline(v=mean(guns.WAS[,"murder"]), col="blue")
par(new=TRUE)
hist(guns.MIN[,"murder"], breaks=15, col="light green", xlim=c(0, 7), ylim=c(0,5), xlab="", main="")
abline(v=mean(guns.MIN[,"murder"]), col="dark green")
legend("topright", legend = c("Washington mean", "Minnesota mean"),text.col=c("blue", "dark green"))

#need to check our three critical assumptions: 1) equal variance, 2) normally distributed, and 3) sampled independently
#Test assumption 1
shapiro.test(guns.WAS[,"murder"])
shapiro.test(guns.MIN[,"murder"])

#Test assumption 2
var.test(guns.WAS[,"murder"], guns.MIN[,"murder"])

#Test assumption 3 isn't likely

#Test the null hypothesis:
t.test(guns.MIN[,"murder"], guns.WAS[,"murder"], paired = FALSE, var.equal = TRUE)

##the results show a p-value of 1.33e-15, which is much smaller than 0.05, so we can confidently reject the null hypothesis that the washington and minnesota murder rates are not different from one another, with a very small probability of committing type I error
