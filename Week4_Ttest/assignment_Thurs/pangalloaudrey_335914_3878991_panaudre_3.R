setwd("C:/Users/Audrey/Desktop/r stuff/week4")
guns<- read.csv("guns_NRE538.csv", header=T, sep = ",", comment.char = "#")
guns.MI= subset(guns, state=="Michigan")
t.test(guns.MI[,"murder"], mu=9)
hist(guns.MI[,"murder"], breaks = 20)
abline(v=mean(guns.MI[,"murder"]), col="chartreuse")
abline(v=9, col="dark green")
#R does not recognize pine, olive, or grass green as actual colors CAN YOU BELIEVE THAT
legend("topleft", legend = c("sample mean", "specified value=9"),text.col=c("chartreuse", "dark green"))
# I needed to make the graph before answering the question. I get confused because "rejecting the null" feels like the opposite term to me
# But I think, with a confidence interval of 9.08 and 10.25, and a p value of 0.02, we've found that Michigan does significantly different from the value 9
# I think here we're rejecting the null since it's significantly different

guns.WAS= subset(guns, state=="Washington")
guns.MIN= subset(guns, state== "Minnesota")
t.test(guns.WAS[,"murder"], guns.MIN[,"murder"], paired = FALSE)
#Wow that is one very tiny p value
#A significant difference means we reject the null and say that the murder rates from guns are not equal
# paired= false is to indicate that these are two different groups of people
t.test(guns.WAS[,"murder"], guns.MIN[,"murder"], paired = TRUE)
t.test(guns.WAS[,"robbery"], guns.MIN[,"robbery"], paired = FALSE)
hist(guns.WAS[,"murder"], breaks=15, col="light blue", xlim=c(0, 7), ylim=c(0,5), xlab="murder rate", main="")
abline(v=mean(guns.WAS[,"murder"]), col="blue")
par(new=TRUE)
hist(guns.MIN[,"murder"], breaks=15, col="light green", xlim=c(0, 7), ylim=c(0,5), xlab="", main="")
abline(v=mean(guns.MIN[,"murder"]), col="dark green")
legend("topright", legend = c("Washington mean", "Minnesota mean"),text.col=c("blue", "dark green"))
#shapiro normality test
shapiro.test(guns.WAS[,"murder"])
shapiro.test(guns.MIN[,"murder"])
var.test(guns.WAS[,"murder"], guns.MIN[,"murder"])
library(Lahman)
data(Batting)
bat15 = subset(Batting, yearID==2015 & AB>200)
bat15$avg = bat15$H/bat15$AB
hist(bat15$avg, breaks=30, freq=FALSE)
lines(density(bat15$avg), col="pink")
shapiro.test(bat15$avg)
qqnorm(bat15$avg); qqline(bat15$avg, col="red")
muA=4
sdA=1
set.seed(32)
A = rnorm(30, mean=muA, sd=sdA)
A.mean=mean(A)

A.mean.SE=sqrt(var(A)/length(A))
A.mean.CI=c(A.mean-qnorm(0.975)*A.mean.SE, A.mean+qnorm(0.975)*A.mean.SE)
xs = seq(0, 15, by =0.01)
fA = dnorm(xs, mean=A.mean, sd=A.mean.SE)
plot(fA~xs, type="l", lwd=3, col="blue", xlim=c(3.5, 5), ylim=c(0, 3), 
     main="distribution of means of sample A", xlab="value", ylab="density")
abline(v=c(qnorm(0.025, mean=A.mean, sd=A.mean.SE), qnorm(0.975, mean=A.mean, sd=A.mean.SE)), col="blue", lty="dashed")
abline(v=4.5, col="dark green", lty="solid", lwd=3)
p.m=2*(1-pnorm(4.5, mean=A.mean, sd=A.mean.SE))
p.m
t.test(A,mu=4.5)$p.value
muA=4
muB=4.5
sdA=1
sdB=1
set.seed(32)
A = rnorm(30, mean=muA, sd=sdA)
B = rnorm(30, mean=muB, sd=sdB)
hist(A,breaks=30,col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,10), xlab="Value", main="histogram of sample A and B")
hist(B,breaks=30,col=rgb(0,1,0,0.5),add=TRUE)
box()
set.seed(9487)
dif.s=c() # creating an empty vector to store the "differences" in the for loop
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
2*(length(which(dif.s>0))/length
(dif.s))
t.test(A, B, paired = FALSE)


#Exercise 2

A = rnorm(30, mean=guns.MIN$murder, sd=guns.MIN$murder)
B = rnorm(30, mean=guns.WAS$murder, sd=guns.WAS$murder)
hist(A,breaks=30,col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,10), xlab="Value", main="histogram of Murders")
hist(B,breaks=30,col=rgb(0,1,0,0.5),add=TRUE)
box()

murderdif=c()
for (i in 1:10000){
murderdif[i]= mean(sample(guns.MIN$murder, length(guns.MIN$murder), replace=TRUE))- 
              mean(sample(guns.WAS$murder, length(guns.WAS$murder), replace=TRUE))
}
murder.s.mean= mean(murderdif)
murdersd= sd(murderdif)
CImurder.lo= sort(murderdif)[10000*0.025]
CImurder.hi= sort(murderdif)[10000*.975]
hist(murderdif, probability = TRUE)
lines(density(murderdif), col= "red")
abline(v=c(CI95.lo, CI95.hi), col="blue", lty="dashed")
abline(v=0, col="dark green", lty="solid", lwd=3)

legend("topright", legend = c("95% confidence interval", 
                              "0"), 
       col=c("blue", "dark green"),
       text.col=c("blue", "dark green"),
       lty=c("dashed", "solid"), bty="n")
2*(length(which(murderdif>0))/length(murderdif))
#The P value is 0, which isn't the same but we talked about how t test p vales cannot be zero.
#So this means we can be fairly certain that the difference in the means in murder rates will fall somewhere within the confidence interval, I think
# We reject the null and say that murder rates are different between Washington and Minnesota