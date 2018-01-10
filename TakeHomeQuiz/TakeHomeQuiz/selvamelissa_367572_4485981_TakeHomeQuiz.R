# Energy data for question 1-4
edata = read.table((file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv"), 
                   sep="," , fill=TRUE, header=TRUE)
View(edata)

# Housing data for question 5-9
hdata = read.table((file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv"), 
                   sep=",", fill=TRUE, header=TRUE)

View(hdata)
#question 1b
library(plyr)
edata$percapita.energy = edata$TotalEnergy / edata$Population
energy.coast = subset(edata, Coast == 1)
energy.nocoast = subset(edata, Coast == 0)
boxplot(percapita.energy ~ Coast, data=edata, main= "Per Capita Energy Consumption by State Location", xlab= "State Location (Coast vs. Not Coast)", ylab= "Per Capita Energy Consumption", ylim= c(0,0.6))

#question 1c
shapiro.test(energy.coast[,"percapita.energy"])
shapiro.test(energy.nocoast[,"percapita.energy"])
var.test(energy.coast[,"percapita.energy"], energy.nocoast[,"percapita.energy"])

#question 1d
t.test(energy.coast$percapita.energy, energy.nocoast$percapita.energy, paired = FALSE)

#question 2b
edata$percapita.coal = edata$TotalCoal / edata$Population
coal.coast = subset(edata, Coast == 1)
coal.nocoast = subset(edata, Coast == 0)
boxplot(percapita.coal ~ Coast, data=edata, main= "Per Capita Coal Consumption by State Location", xlab= "State Location (Coast vs. Not Coast)", ylab= "Per Capita Coal Consumption", ylim= c(0,0.25))

#question 2c
shapiro.test(coal.coast[,"percapita.coal"])
shapiro.test(coal.nocoast[,"percapita.coal"])
var.test(coal.coast[,"percapita.coal"], coal.nocoast[,"percapita.coal"])

#question 2d
t.test(coal.coast$percapita.coal, coal.nocoast$percapita.coal, paired = FALSE)

#question 3b
boxplot(percapita.coal ~ Region, data=edata, main = "Per Capita Coal Consumption by Region", ylim = c(0, 0.3))

#question 3c
shapiro.test(edata$percapita.coal)
leveneTest(percapita.coal~Region, data=edata)

#question 3d
percapitacoal1 = aov(percapita.coal ~ Region, data=edata)
summary(percapitacoal1)
TukeyHSD(percapitacoal1)
library(lmtest)
bptest(percapitacoal1)
shapiro.test(residuals(percapitacoal1))
kruskal.test(percapita.coal ~ Region, data=edata)

#question 4
edata$percaptita.GDP = edata$TotalGDP / edata$Population
cor(edata$percapita.coal, edata$percaptita.GDP)
plot(edata$percapita.coal, edata$percaptita.GDP)

#question5
cor(hdata[,c(1,6,8)])
library(car)
vif(lm(medv~rm+dis+crim, data=hdata))

#question 6
plot(hdata$rm, hdata$medv)
plot(hdata$dis, hdata$medv)
plot(hdata$crim, hdata$medv)

#question 7
house.value = lm(medv ~ crim + rm + dis, data= hdata)
summary(house.value)
resid.house = resid(house.value)
plot(resid.house)
library(lmtest)
dwtest(house.value, alternative=c("two.sided"))
bptest(house.value)
shapiro.test(resid(house.value))
qqnorm(resid(house.value))
qqline(resid(house.value), col = "red")
