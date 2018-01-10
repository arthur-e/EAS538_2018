# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

head(edata)
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)

e.coast = subset(edata, Coast=="1")
e.noncoast = subset(edata, Coast=="0")

##Q1
hist(e.coast[,"TotalEnergy"], col="light blue", breaks=10, xlim=c(150000, 5000000), ylim=c(0,10), main="Total Energy in Coastal vs. Non-Coastal States", ylab="Number of States", xlab="Total Energy")
abline(v=mean(e.coast[,"TotalEnergy"]), col="blue")
par(new=TRUE)
hist(e.noncoast[,"TotalEnergy"], breaks=10, col="pink", xlim = c(150000, 5000000), ylim=c(0,10), xlab = "", ylab="", main="")
abline(v=mean(e.noncoast[,"TotalEnergy"]), col="red")
legend("topright", legend = c("Coastal mean", "Non-coastal mean"),text.col=c("blue", "red"))

#Check for equal variance
var.test(e.coast$TotalEnergy, e.noncoast$TotalEnergy)
# p-value = 1.083e-05, does not have equal variance

#Check for normality
shapiro.test(e.coast$TotalEnergy)
#p-value = 1.802e-05, not normally distributed
shapiro.test(e.noncoast$TotalEnergy)
#p-value = 0.006032, not normally distributed

t.test(e.coast$TotalEnergy, e.noncoast$TotalEnergy)
#p-value: 0.1494

##Q2
#Create per capita column
edata = edata %>%
  mutate(percapita=(TotalEnergy/Population))
head(edata)

e.coast = subset(edata, Coast=="1")
e.noncoast = subset(edata, Coast=="0")

hist(e.coast[,"percapita"], col="light blue", breaks=20, xlim=c(0.1, 1), ylim=c(0,10), main="Per Capita Energy in Coastal vs. Non-Coastal States", ylab="Number of States", xlab="Total Energy")
par(new=TRUE)
hist(e.noncoast[,"percapita"], breaks=20, col="pink", xlim = c(0.1, 1), ylim=c(0,10), xlab = "", ylab="", main="")
abline(v=mean(e.coast[,"percapita"]), col="blue")
abline(v=mean(e.noncoast[,"percapita"]), col="red")
legend("topright", legend = c("Coastal mean", "Non-coastal mean"),text.col=c("blue", "red"))

#Check for equal variance
var.test(e.coast$percapita, e.noncoast$percapita)
# p-value = 0.5098, equal variance assumed

#Check for normality
shapiro.test(e.coast$percapita)
#p-value = 5.037e-06, not normally distributed
shapiro.test(e.noncoast$percapita)
#p-value = 3.627e-05, not normally distributed

t.test(e.coast$percapita, e.noncoast$percapita)
#p-value = 0.2245

##Q3
edata = edata %>%
  mutate(PCcoal=(TotalCoal/Population))
head(edata)

boxplot(PCcoal~Region, data=edata)

#Check for normality
shapiro.test(edata$PCcoal)
#p-value = 6.024e-11, not normally distributed
shapiro.test(edata$PCGDP)
#p-value = 1.311e-09, not normally distributed

install.packages("Rcmdr")
library(Rcmdr)

#Check variance
leveneTest(PCcoal~Region, data=edata)
#Variance can be assumed equal, p=0.5202

coal.mod=lm(PCcoal~Region, data=edata)

summary(coal.mod)

#Q4
edata = edata %>%
  mutate(PCGDP=(TotalGDP/Population))
head(edata)

cor(edata$PCcoal, edata$PCGDP)

cor(edata[,c(4:10)], use="na.or.complete")


# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

#Q5
cor(hdata[,c(1,8,11)])

h.mod=lm(medv~crim+dis+ptratio, data=hdata)

vif = 1/(1 - summary(h.mod)$r.squared)
vif

#Q6
plot(medv~crim, data=hdata)
plot(medv~dis, data=hdata)
plot(medv~ptratio, data=hdata)


#Q7
summary(h.mod)

plot(h.mod)
#QQplot shows residuals are clearly not normal.

dwtest(h.mod)
#DW = 0.38196, p-value < 2.2e-16, data is autocorrelated

bptest(h.mod)
#p-value = 2.316e-08, data is heteroscedastic

#Try to identify which data to transform:
hist(hdata$crim, breaks = 40) #log transformation may be appropriate
hist(hdata$dis, breaks = 40) #log transformation may also help in this case
hist(hdata$ptratio, breaks = 40) #??

##Check:
trans.crim=log(hdata$crim)
hist(trans.crim, breaks=40)

qqnorm(trans.crim)
qqline(trans.crim, col="red")

qqnorm(hdata$crim)
qqline(hdata$crim, col="red")

#Still not perfect, but better. 

trans.dis=log(hdata$dis)
hist(trans.dis, breaks=40)

qqnorm(trans.dis)
qqline(trans.dis, col="red")

qqnorm(hdata$dis)
qqline(hdata$dis, col="red")

##Looks slightly better...

hdata = hdata %>%
  mutate(trans.crim=log(crim))

hdata = hdata %>%
  mutate(trans.dis=log(dis))

head(hdata)

h.mod2=lm(medv~trans.crim+trans.dis+ptratio, data=hdata)

qqPlot(h.mod2)
qqPlot(h.mod)

##...Not a lot of improvement.

#Q8 & 9
summary(h.mod)

int.h.mod=lm(medv~ptratio*crim+dis, data = hdata)
summary(int.h.mod)



