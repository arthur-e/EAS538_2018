## set working directory
setwd("/docs/umich/coursework/538/labs/lab04")

## import and subset data
install.packages("RCurl")
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")

## exercise 1 resubmission
## null hypothesis: Michigan's rate of violent crime is less than 600 per 10^5 residents
t.test(guns.MI[,"violent"], mu=600, alternative = "greater")
## the null hypothesis is rejected


## exercise 2 resubmission
## subset and resample Washington state data
guns.WAS = subset(guns, state=="Washington")
guns.WAS.re = c()
for (i in 1:10000){
  guns.WAS.re[i]=mean(sample(guns.WAS$murder,nrow(guns.WAS),replace=TRUE))
}

## subset and resample Minnesota state data
guns.MIN = subset(guns, state=="Minnesota")
guns.MIN.re = c()
for (i in 1:10000){
  guns.MIN.re[i]=mean(sample(guns.MIN$murder,nrow(guns.MIN),replace=TRUE))
}

## calculate the mean difference between paired values in the distributions
diff.mean = mean(guns.WAS.re - guns.MIN.re)

## calculate the two-tailed p-value by dividing the 
diff.p = 2*(length(which(diff.mean>0))/length(diff.mean))
## the null hypothesis is rejected