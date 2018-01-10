guns<- read.csv("guns.csv", header = TRUE)
guns.MI = subset(guns, state=="Michigan")

###EXERCISE 1
### Null Hypothesis: The actual robbery rate is equal to 300/10^5 people in MI
t.test(guns.MI[,"robbery"], mu=300)
hist(guns.MI[,"robbery"], breaks=50)
abline(v=mean(guns.MI[,"robbery"]), col="blue")
abline(v=300, col="dark green")
legend("topleft", legend = c("sample mean", "specified value=300"),text.col=c("blue", "dark green"))
### Result: The sample mean is not equal to the specified value, i.e, the robbery rate (in our sample) is lower than 300 per 10^5 people. Hence the null hypothesis is not valid. 

###EXERCISE 2
### Null hypothesis: the murder rate in washington and Minnesota are not different from each other. 
### that is, find the p value for which the murder rates in the two states are the same!
guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")
set.seed(230)
dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS$murder, 23, replace=TRUE)) - mean(sample(guns.MIN$murder, 23, replace=TRUE))
}
dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]
p.val= 2*(length(which(dif.s>0))/length(dif.s))
p.val
### p value for rejecting the null hypothesis is 2. 
t.test(guns.WAS$murder,guns.MIN$murder, paired= FALSE)
