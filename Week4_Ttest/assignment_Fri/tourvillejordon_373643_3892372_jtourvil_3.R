###EXERCISE 1
setwd("//TOURVILLE/Users/jtourvil/Desktop/Stats R")
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")
mean_rob=mean(guns$robbery)
t.test(guns.MI[,"robbery"], mu=mean_rob)#Ho=mean of Michigan robberies is not significantly different from the national mean.
#t=7.4125
#df=22
#p=0.0000002045
#95% Confidence interval: 213.7522, 254.1
#mean of x=233.9261
#Reject Ho, mean Michigan robberies is significantly different (lower) from the national average.
###
###EXERCISE 2
guns.WAS = subset(guns, state=="Washington", select=3)
guns.MIN = subset(guns, state=="Minnesota", select=3)
means_MIN = (guns.MIN[, 1])
means_WAS = (guns.WAS[, 1])
set.seed(1)#Resample method
dif.s=c() 
for (i in 1:10000){
  dif.s[i]=mean(sample(means_MIN, 30, replace=TRUE)) - mean(sample(means_WAS, 30, replace=TRUE))
}
dif.s.mean = mean(dif.s)
dif.s.sd = sd(dif.s)
CI95.lo = sort(dif.s)[10000*0.025]
CI95.hi = sort(dif.s)[10000*0.975]
2*(length(which(dif.s>0))/length(dif.s))
#Returns a value of 0, Ho rejected.
t.test(means_MIN, means_WAS, paired = FALSE)
#Reject Ho, p=0.000000000000001546. Close to manually calculated value.
###