edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
      sep=",", fill=TRUE, header=TRUE)

percapita = edata$TotalEnergy/edata$Population
head(percapita)
finaledata=cbind(edata, percapita)

#Q1.Two-sample t test
#1. Per capita energy cons=DV (continuous); on coast = IV, categorical
ecoast=subset(finaledata, Coast=="1")
head(ecoast)
enocoast=subset(finaledata, Coast=="0")
head(enocoast)
shapiro.test(ecoast$percapita)
# Data is not normally distributed since p<0.05
shapiro.test(enocoast$percapita) 
# Data is not normally distributed since p<0.05
qqnorm(ecoast$percapita)
var.test(ecoast$percapita, enocoast$percapita)
hist(ecoast[,"percapita"], col="light blue", breaks=15, xlim=c(0,1), ylim=c(0,9), xlab="per capita energy consumption", ylab="frequency", main="Histogram of Per Capita Energy Distribution in Coastal and Noncoastal States")
abline(v=mean(ecoast[,"percapita"]), col="blue")
par(new=TRUE)
hist(enocoast[,"percapita"], col="light green", breaks=15, xlim=c(0,1), ylim=c(0,9), xlab="", ylab="", main="")
abline(v=mean(enocoast[,"percapita"]), col="forest green")
  legend("topright", legend=c("Coastal mean", "Noncoastal mean"), text.col=c("blue", "dark green"))
wilcox.test(percapita ~ Coast, data=finaledata)

#Q2. DV = per capita coal (contin); IV = coast (categorical)
pccoal = edata$TotalCoal/edata$Population
head(pccoal)
finaledata2=cbind(edata, pccoal)
ecoast2=subset(finaledata2, Coast=="1")
head(ecoast2)
enocoast2=subset(finaledata2, Coast=="0")
head(enocoast2)
shapiro.test(ecoast2$pccoal)
shapiro.test(enocoast2$pccoal) 
var.test(ecoast2$pccoal, enocoast2$pccoal)
hist(ecoast2[,"pccoal"], col="light blue", breaks=30, xlim=c(0,0.5), ylim=c(0,9), xlab="per capita coal consumption", ylab="frequency", main="Histogram of Per Capita Coal Consumption in Coastal and Noncoastal States")
abline(v=mean(ecoast2[,"pccoal"]), col="blue")
par(new=TRUE)
hist(enocoast2[,"pccoal"], breaks=30, xlim=c(0,0.5), ylim=c(0,9), xlab="", ylab="", main="")
abline(v=mean(enocoast2[,"pccoal"]), col="black")
legend("topright", legend=c("Coastal mean", "Noncoastal mean"), text.col=c("blue", "black"))
t.test(pccoal~Coast, data=finaledata2, paired=FALSE, alternative=c("two.sided"))

#Q3
str(finaledata2)
boxplot(pccoal~Region, data=finaledata2, xlab="Region", ylab="Mean Per Capita Coal Consumption (Effect Size)", main="Boxplot of Mean Per Capita Coal Consumption in US")
east=subset(finaledata2,Region=="East")
shapiro.test(east$pccoal)
west=subset(finaledata2, Region=="West")
shapiro.test(west$pccoal)
midwest=subset(finaledata2, Region=="Midwest")
shapiro.test(midwest$pccoal)
south=subset(finaledata2, Region=="South")
shapiro.test(south$pccoal)
region.aov=lm(pccoal~Region-1, data=finaledata2)
summary(region.aov)
install.packages('car')
library(car)
leveneTest(pccoal~Region, data=finaledata2)
aov=aov(pccoal~Region, data=finaledata2)
summary(aov)

#Q4
pcgdp = edata$TotalGDP/edata$Population
head(pcgdp)
finaledata4=cbind(finaledata2, pcgdp)
pairs(finaledata4[,c("pcgdp", "pccoal")])
cor(finaledata4[,c("pcgdp", "pccoal")])

#5
# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
pairs(hdata[,c(1,8,11)])
cor(hdata[,c(1,8,11)])
vif=1/(1-summary(lm(medv~crim+dis+ptratio, data=hdata))$r.squared)
vif


#6
plot(medv~crim, data=hdata)
plot(medv~dis, data=hdata)
plot(medv~ptratio, data=hdata)

#7
mod.1=lm(medv~crim+dis+ptratio, data=hdata)
summary(mod.1)
install.packages('dplyr')
library(dplyr)
install.packages('lmtest')
library(lmtest)
plot(residuals(mod.1))
dwtest(mod.1, alternative=c("two.sided"))
bptest(mod.1)
shapiro.test(residuals(mod.1))
qqnorm(hdata[,"crim"])
qqnorm(hdata[,"ptratio"])
qqnorm(hdata[,"dis"])
#Tranforming the data
hdata = hdata %>%
  mutate(crim.log=log(crim))
qqnorm(hdata[,"crim.log"])
qqline(hdata[,"crim.log"], col="red")
shapiro.test(hdata[,"crim.log"])
hdatab = hdata %>%
  mutate(dis.log=log(dis))
qqnorm(hdatab[,"dis.log"])
qqline(hdatab[,"dis.log"], col="red")
shapiro.test(hdatab[,"dis.log"])
hdatac = hdata %>%
  mutate(ptratio.log=log(ptratio))
qqnorm(hdatac[,"ptratio.log"])
qqline(hdatac[,"ptratio.log"], col="red")
shapiro.test(hdatac[,"ptratio.log"])
mod.2=lm(medv~crim.log+dis.log+ptratio.log, data=hdata)
summary(mod.2)


