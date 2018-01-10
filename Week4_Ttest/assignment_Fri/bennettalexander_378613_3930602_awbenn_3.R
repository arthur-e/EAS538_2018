## set working directory
setwd("/docs/umich/coursework/538/labs/lab04")

## import and subset data
install.packages("RCurl")
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")

## exercise 1
## one-tailed test
## null hypothesis: Michigan's rate of violent crime is less than or equal to 600 per 10^5 residents
t.test(guns.MI[,"violent"], mu=600, alternative = "greater")
## the null hypothesis is rejected

## two-tailed test
## null hypothesis: Michigan's rate of violent crime is equal to 600 per 10^5 residents
t.test(guns.MI[,"violent"], mu=600)
## the null hypothesis is rejected


## exercise 2
## subset data
guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")

## calculate numerator for t-statistic formula
numerator = mean(guns.WAS$murder)-mean(guns.MIN$murder)-0

## calculate pooled standard error
sp = sqrt((22*sd(guns.WAS$murder)^2+22*sd(guns.MIN$murder)^2)/(44))

## calculate t-statistic using the numerator and sp
tstat = numerator/(sp*sqrt(1/23 + 1/23))

## calculate p-value using the distribution of the t-statistic
pval = (1 - pt(tstat,44))*2
## the null hypothesis is rejected