setwd("D:/UM/SNRE/Winter 2017/NRE 538 Statistics/Week 4 Lab")
guns=read.csv("guns_NRE538.CSV",sep=",",header=T,comment.char="#")

#Exercise 1
#Compose a one-tail and a two-tail null hypotheses on another statistics of Michigan 
#state and perform t-test to test the two hypotheses.
guns.MI=subset(guns,state=="Michigan")

#Two-tailed 
#H0: the violent crime rate of Michigan state is not significantly different from 700 incidents per 10^5 members of population
t.test(guns.MI[,2],mu=700)
#p-value>0.05 and we fail to reject null hypothesis
#Conclusion: Violent crime rate of Michigan state is not significantly different from 700 incidents per 10^5 members of population

#One-tailed
#H0: the violent crime rate of Michigan state is significantly greater than or euqal to 750 incidents per 10^5 members of population.
t.test(guns.MI[,2],alternative=c("less"),mu=750)
#p-value<0.05, and we can reject null hypothesis.
#Conclusion: the violent crime rate of Michigan state is significantly less than 750 incidents per 10^5 members of population

#Exercise 2: Calculate the p-value of rejecting the following null hypothesis
#H0: The murder rate of Washington state and murder rate of Minnesota state are not different from each other
guns.WAS=subset(guns,state=="Washington")
guns.MIN=subset(guns,state=="Minnesota")

#Method 1
wasavg=mean(guns.WAS[,3])
wassd=sd(guns.WAS[,3])
minavg=mean(guns.MIN[,3])
minsd=sd(guns.MIN[,3])

var.test(guns.WAS[,3],guns.MIN[,3])
#The variance of two samples are not significantly different, so they can be pooled
sp=sqrt((22*wassd^2+22*minsd^2)/44)
tstat=(wasavg-minavg)/(sp*sqrt(1/23+1/23))
p=2*(1-pt(tstat,df=44))
p
#The manually calculated p-value is 1.33e-15

#Method 2
set.seed(30)
dif.s=c()
for (i in 1:5000){
  dif.s[i]=mean(sample(guns.WAS[,3], 18, replace=TRUE)) - mean(sample(guns.MIN[,3], 18, replace=TRUE))
}

p=2*(length(which(dif.s<0))/length(dif.s))
p
#The p-value calculated from this method is 0 (indicating it is very small)

t.test(guns.WAS[,3],guns.MIN[,3],paired = FALSE)
#The calcuation from t-test is 1.546e-15.

#The first method got the closest to the t-test and all three results are very close
#The 0 calculated by resampling probably indicates that the p-value is very small