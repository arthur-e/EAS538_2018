setwd("/afs/umich.edu/user/w/a/wayihan")
#create data of the pupolation
Truemean=5
truesd=2
population=rnorm(10000, Truemean, truesd)
hist(population)

# get data for the sample
N=100
> sampleN = sample(population, N)
> mean = mean(sampleN)
> sd = sd(sampleN)
> se = sd/(sqrt(N))
> error = qnorm(0.975)*se # 95% confidence interval
> left = mean-error
> right = mean+error
> range = c(lef, right)
Error: object 'lef' not found
> range = c(left, right)
> range
[1] 4.735266 5.469727
> ####
  fert=read.csv("lecture6_fertilizer.csv")
setwd("/Users/yihanwang/Desktop")
fert=read.csv("lecture6_fertilizer.csv")
manual = subset(fert, Type=="m")
spreader = subset(fert, Type=="s")

 # two-tailed t test
t. test(flintlead, mu=15) # one sample, two-tailed t test
t. test(flintlead, mu=15, alte
        rnative = "greater") # one sample, one-tailed t test

#Week4 Lab
setwd("/Users/yihanwang/Desktop")
library(RCurl)
guns=read.csv("/Users/yihanwang/Downloads/guns_NRE538.csv",sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")

library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[, "murder"], mu=9)
hist(guns.MI[, "murder"], breaks=20)
abline(v=mean(guns.MI[, "murder"]), col="blue")
abline(v=9, col="dark green")
legend("topleft", legend=c("sample mean", "specified value=9"), test.col=c"blue", "dark green")

#Excersie 1
#one-tail:
#H0: The robbery rate is significantly greater than 300 per 10^5 people in Michigan state. 
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI$robbery, mu=300, alternative="less")
#the t-test shows that we are confident to reject H0, which means that the robbery rate is significantly less than 300 per 10^5 people in michgian state.

#two-tail:
#H0: The number of prisoners is not different from 350 per 10^5 people in Michigan state
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI$prisoners, mu=350)
#The result shows that the mean of our sample is significantly different from 350 per 10^5 people in Michigan State. We reject H0. 

#Exercise 2:
#two sample t-test:
shapiro.test(guns.WAS[, "murder"])
shapiro.test(guns.MIN[, "murder"])
#Shapiro test shows that the two data frame follow normal distribution
var.test(guns.WAS[, "murder"], guns.MIN[, "murder"])
#the F test shows P value is greater than 0.05. We fail to reject the null hypothesis, which means that the two variances are the same
#The assumptions for t-test are met, so the results from this unpaired t-test should be robust. guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")
t.test(guns.WAS[, "murder"], guns.MIN[, "murder"], paired = FALSE)
#the p value is less than 0.05, so we reject H0, which means that the murder rate in two states are significantly different from each other.

