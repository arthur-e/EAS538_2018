##Excercise One
library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#") 
guns.MI = subset(guns, state=="Michigan")
##Two tail
##Null hypothesis: the number of violent crimes is not equal to 500 per 10^5 people
t.test(guns.MI[,"violent"], mu=500)
## The null hypothesis can be rejected

##One tail
##Null hypothesis: the number of violent crimes is greater than 500 per 10^5
t.test(guns.MI[,"violent"], mu=500, alternative="greater")

##yes, the mean is greater than 500

##Excercise 2
guns.MN = subset(guns, state=="Minnesota")
guns.WA = subset(guns, state=="Washington")
shapiro.test(guns.MN[,"murder"])
shapiro.test(guns.WA[,"murder"])
var.test(guns.MN[,"murder"], guns.MN[,"murder"])
##Critical assumptions are met
t.test(guns.MN[,"murder"], guns.MN[,"murder"], paired = FALSE, var.equal = TRUE)
