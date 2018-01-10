library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")

## Exercise 1
# One tail: H0 - Michigan's violent crime ($violent) rate 
# is less than 750 per 10^5 people 
t.test(guns.MI[,"violent"], mu=650,alternative="greater")
# result: p = 0.0067, 95% CI: [666.3 inf] so for alpha = 0.05, p < alpha
# reject H0, Michigan's avg violent crime rate is greater than 650 per 10^5 people

guns.WAS = subset(guns, state=="Washington") # normal dist
guns.MIN = subset(guns, state=="Minnesota") # normal dist
t.test(guns.WAS[,"murder"],guns.MIN[,"murder"],paired=FALSE)
# var.test(guns.WAS[,"murder"],guns.MIN[,"murder"]) # p-val = 0.66, do not reject H0 (same var)


## Exercise 2

set.seed(9487)
dif.s=c() # creating an empty vector to store the "differences" in the for loop
for (i in 1:10000){
  dif.s[i]=mean(sample(guns.WAS[,"murder"], 30, replace=TRUE)) - mean(sample(guns.MIN[,"murder"], 30, replace=TRUE))
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
(length(which(dif.s>0))/length(dif.s))
# do not reject H0 because 1=p>0.05
