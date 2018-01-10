###EXERCISE 1

#two-tail null hypothesis (H0): The violent crime rate in Michigan is not significantly different from 20 incidents per 10^5 people.

install.packages('RCurl')
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[,"violent"], mu=20)

#the p-value is < 2.2e-16, meaning that we can reject the null hypothesis that the
#violent crime rate in Michigan is not significantly different from 20 incidents per 10^5 people.



#one-tail null hypothesis (H0): The violent crime rate in Michigan is not greater than 20 incidents per 10^5 people.
t.test(guns.MI[, "violent"], mu=20, alternative="greater")
#p-value < 2.2e-16, meaning that we can reject the null hypothesis that the violent crime
#rate in Michigan is not greater than 20 incidents per 10^5 people.


###EXERCISE 2


set.seed(9487)
dif.s=c()
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS[, "murder"], 23, replace=TRUE)) - mean(sample(guns.MIN[, "murder"], 23, replace=TRUE))
}
p = 2*(length(which(dif.s<0))/length(dif.s))
#p-value = 0
#This means we can reject the null hypothesis that the murder rate of Washington state
#and of Minnesota are not different from each other.

#calculate p-value using t-test for comparison:
guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")
t.test(guns.WAS[, "murder"], guns.MIN[, "murder"], paired=FALSE)
#p-value = 1.546e-15, which is very close to zero
