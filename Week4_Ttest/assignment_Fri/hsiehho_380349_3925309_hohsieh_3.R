#Ho Hsieh_wk4_lab3

####Exercise 1####

#One-tail one-sample t-test
#Question: Is the robbery rate in Michigan significantly different from 300 per 10^5 people?
#H0: u0(300)=u1; Ha: u0(300)=/=u1

guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"robbery"], mu=300)
#t = -6.7924, df = 22, p-value = 7.974e-07
# p-value<<0.05 which means the robbery rate in Michigan is significantly different from 300 per 10^5 people


#Two-tail one-sample t-test
#Question: Is the robbery rate in Michigan significantly greater than 300 per 10^5 people?
#H0: u1<=u0(300); Ha: u1>u0(300)

t.test(guns.MI[, "robbery"], mu=300, alternative = "greater")
#t = -6.7924, df = 22, p-value = 1
#p-value>0.05 which means the robbery rate in Michigan is not significantly greater than 300 per 10^5 people


####Exercise 2####
library(RCurl)
guns = read.csv(text = getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep = ",",header = T, comment.char = "#")

guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")

##Method 1: t-test (not resampling)#################################

#check the assumptions
shapiro.test(guns.WAS[,"murder"]) #W = 0.94148, p-value = 0.1933 #can't reject null-->(can't say it's not normal distribution)
shapiro.test(guns.MIN[,"murder"]) #W = 0.98013, p-value = 0.9083 #can't reject null--->(can't say it's not normal distribution)
var.test(guns.WAS[,"murder"], guns.MIN[,"murder"]) 
#F = 1.2072, num df = 22, denom df = 22, p-value = 0.6626 
#can't reject null-->(can't say they do not have the same variance)

mean.WAS=mean(guns.WAS[,"murder"])
mean.MIN=mean(guns.MIN[,"murder"])
sd.WAS=sd(guns.WAS[,"murder"])
sd.MIN=sd(guns.MIN[,"murder"])
n.WAS=length(guns.WAS[,"murder"]) 
n.MIN=length(guns.MIN[,"murder"])
se.pooled=(sqrt(1/n.WAS+1/n.MIN)
           *sqrt(((n.WAS-1)*sd.WAS^2+(n.MIN-1)*sd.MIN^2)
                 /(n.WAS+n.MIN-2)))
t= (mean.WAS-mean.MIN)/se.pooled
pval=2*(1-pt(t, 44))
pval 
#p value=1.332268e-15<<0.05 which means we can reject null hypothesis.
#and the murder rate in Washington is significantly different from that in Minnesota.


#use t.test to verify the result
t.test(guns.WAS[,"murder"], guns.MIN[,"murder"],var.equal = T)
#t = 12.104, df = 44, p-value = 1.347e-15




##Method 2: resampling################################
dif =c()

for (i in 1:10000){
  dif[i] = mean(sample(guns.WAS[,"murder"], 23, replace = T))-mean(sample(guns.MIN[,"murder"],23, replace = T))
} 

dif.mean=mean(dif)
dif.sd=sd(dif)
CI95.lo=sort(dif)[10000*0.025]
CI95.hi=sort(dif)[10000*0.975]
CI95=c(CI95.lo,CI95.hi) #1.76087 2.43913

hist(dif, probability = T, xlim = c(-1,7) ,main = "distribution of differences between murder rate in WA and MIN")
lines(density(dif), col="red")
abline(v=CI95, col="blue",lty="dashed" )
abline(v=0, col="dark green", lwd=3)
legend("topleft", legend = c("95% confidence interval", "0"), col = c("blue", "dark green"), text.col = c("blue", "dark green"), lty=c("dashed","solid"), bty="o")

pval= 2*(length(which(dif<0))/length(dif)) 
pval #p-value = 0
#p-value<<0.05 which means we can reject null hypothesis
#and the murder rate in Washington is significantly different from that in Minnesota.
