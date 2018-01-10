setwd("C:/Users/szczeb/Desktop/NRE 538_Labs/Lab 3")
guns = read.table("guns_NRE538.csv",header=T,fill=T,sep=",")
head(guns)

##Exercise 1
#Null hypothesis for two-tailed--> Michigan state's violence rate is equal to 650 incidents per 10^5 people
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"violent"], mu=650)
#the p-value is 0.0134, which is less than 0.05, so we can reject the null hypothesis with little probably of committing a type I error.

##Exercise 2
#Null hypothesis: the murder rate of Washington state and murder rate of Minnesota state are not different from each other
guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")

muguns.WAS$murder = 7
muguns.MIN$murder = 8
sdguns.WAS$murder = 2
sdguns.MIN$murder = 2

mean(guns.WAS$murder)

set.seed(50)
guns.WAS$murder = rnorm(23, mean=muguns.WAS$murder, sd=sdguns.WAS$murder)
guns.MIN$murder = rnorm(23, mean=muguns.MIN$murder, sd=sdguns.MIN$murder)

hist(guns.WAS$murder,breaks=30,col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,10), xlab="Value", main="histogram of sample guns.WAS$murder and guns.MIN$murder")
hist(guns.MIN$murder,breaks=30,col=rgb(0,1,0,0.5),add=TRUE)
box()

set.seed(50)
dif.s=c() 
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS$murder, 50, replace=TRUE)) - mean(sample(guns.MIN$murder, 50, replace=TRUE))
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

##answer of 0

#confirm calculation

t.test(guns.WAS$murder, guns.MIN$murder, paired=FALSE)

## answer of 0.00079

##Since the p-value is far less than 0.05, we can reject the null hypothesis that the murder rates in WAS and MIN are not different from one another, knowing the probability of committing a type I error is very small.

