###Quiz 4 Take Home
###Lauren Edson

# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(edata)


TotEnergyCap=edata$TotalEnergy/edata$Population
TotCoalCap=edata$TotalCoal/edata$Population
TotGDPCap=edata$TotalGDP/edata$Population

#1
boxplot(edata$Coast, TotEnergyCap, ylab="Per Capita Energy Consumption", xlab="State Location", names=c("Not Coast", "Coast"))
var.test(edata$Coast,TotEnergyCap)
#variances are not equal; use welch's t test
t.test(edata$Coast,TotEnergyCap1, altrenative = "two.sided")

#2
boxplot(edata$Coast, TotCoalCap, ylab="Per Capita Coal Consumption", xlab="State Location", names=c("Not Coast", "Coast"))
var.test(edata$Coast,TotCoalCap)
#variances are not equal; use welch's t test
t.test(edata$Coast,TotEnergyCap1, altrenative = "two.sided")

#3
boxplot(TotCoalCap~edata$Region, ylab="Per Capita Coal Consumption", xlab="Region", names=c("South", "West", "East", "Midwest"))

library(car)
leveneTest(TotCoalCap~edata$Region)

runtest=aov(TotCoalCap~edata$Region)
summary(runtest)

#4
cor(TotCoalCap, TotGDPCap)
#correlation = 0.0359

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
#1
pairs(hdata[,c(1,6,8)])
cor(hdata$crim, hdata$rm) #-0.1424577
cor(hdata$rm, hdata$dis) #0.1387741
cor(hdata$crim, hdata$dis) #-0.4619674

 
#2
plot(medv~crim, data=hdata)
plot(medv~rm, data=hdata)
plot(medv~dis, data=hdata)

#3
housemodel=lm(medv~ crim+dis+rm, data=hdata)
summary(housemodel)
plot(housemodel)


#check assumptions
library(lmtest)
dwtest(housemodel, alternative=c("two.sided"))


plot(residuals(housemodel)~fitted(housemodel))
abline(lm(residuals(housemodel)~fitted(housemodel)), col="red")
bptest(housemodel)

qqnorm(residuals(housemodel))
qqline(residuals(housemodel), col="red")
shapiro.test(residuals(housemodel))

#interaction
housemodel2=lm(medv~ crim+rm+crim*rm, data=hdata)
summary(housemodel2)
plot(housemodel2)
