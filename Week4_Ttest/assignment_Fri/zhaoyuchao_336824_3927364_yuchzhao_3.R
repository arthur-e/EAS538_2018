##Exercise 1
setwd("~/Desktop/LAB4")
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")

## Null Hypotheis: the robbery rate is significantly less than or equal to 300 per 10^5 people in Michigan state.
## Alternative: robbery rate is significantly greater than 300 per 10^5 people in Michigan state.

guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[, "robbery"],mu=300,alternative='greater')

##One Sample t-test

                  data:  guns.MI[, "robbery"]
                  t = -6.7924, df = 22, p-value = 1
                  alternative hypothesis: true mean is greater than 300
                  95 percent confidence interval:
                     217.2223      Inf
                  sample estimates:
                  mean of x 
                     233.9261 

## Because p-value>0.05, therfore I fail to reject the null hypothesis. 
## Which means it is ture that the robbery rate is significantly less than or equal to 300 per 10^5 people in Michigan state.   
                     
## Exercise 2
guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota") 
murder.WAS = guns.WAS[, "murder"]
murder.MIN = guns.MIN[, "murder"]
differences=c()
for (i in 1:10000){
differences[i]=mean(sample(murder.WAS, 23, replace=TRUE)) - mean(sample(murder.MIN, 23, replace=TRUE))
}
differences.mean = mean(differences)
differences.sd = sd(differences)
CI95.lo = sort(differences)[10000*0.025]
CI95.hi = sort(differences)[10000*0.975]
2*(length(which(differences>0))/length(differences))

## reseult is 2

                     
                    
                     

                     
                    