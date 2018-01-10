setwd("C:/Users/helen/Documents/winter_2017/538_stats")
library(dplyr)

# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(edata)
str(edata)

#Q1. Does per capita energy consumption differ depending on whether a state is found on the coast or not?
edata = edata %>%
  mutate(TEp.cap = (edata$TotalEnergy / edata$Population))
head(edata)
coast = subset(edata, Coast=="1")
inland = subset(edata, Coast=="0")

#1.b: visual plot

hist(coast[,"TEp.cap"], breaks=15, col="light blue", xlab="energy consumption per capita", main="")
abline(v=mean(coast[,"TEp.cap"]), col="blue")
par(new=TRUE)
hist(inland[,"TEp.cap"], breaks=15, col="light green", xlab="", main="")
abline(v=mean(inland[,"TEp.cap"]), col="dark green")
legend("topright", legend = c("coast mean", "inland mean"),text.col=c("blue", "dark green"))

#1.c: assumptions
shapiro.test(coast$TEp.cap)
qqnorm(coast$TEp.cap); qqline(coast$TEp.cap, col="Red")
shapiro.test(inland$TEp.cap)
qqnorm(inland$TEp.cap); qqline(inland$TEp.cap, col="Red")

var.test(coast$TEp.cap, inland$TEp.cap)

#1.d: test and analysis
wilcox.test(edata$TEp.cap, edata$Coast, paired = FALSE)

#Q.2: Does per capita coal consumption differ depending on whether a state is found on the coast or not?
edata = edata %>%
  mutate(Cp.cap = (edata$TotalCoal / edata$Population))
head(edata)
coast = subset(edata, Coast=="1")
inland = subset(edata, Coast=="0")
#2.b: visual plot
hist(coast[,"Cp.cap"], breaks=15, col="light blue", xlab="coal consumption per capita", main="")
abline(v=mean(coast[,"Cp.cap"]), col="blue")
par(new=TRUE)
hist(inland[,"Cp.cap"], breaks=15, col="light green", xlab="", main="")
abline(v=mean(inland[,"Cp.cap"]), col="dark green")
legend("topright", legend = c("coast mean", "inland mean"),text.col=c("blue", "dark green"))

#2.c: assumptions
shapiro.test(coast$Cp.cap)
qqnorm(coast$Cp.cap); qqline(coast$Cp.cap, col="Red")
shapiro.test(inland$Cp.cap)
qqnorm(inland$Cp.cap); qqline(inland$Cp.cap, col="Red")

var.test(coast$Cp.cap, inland$Cp.cap)

#2.d: test and analysis
wilcox.test(edata$Cp.cap, edata$coast, paired = FALSE)

#Q.3: Does per capita coal consumption differ depending on the region in which a state is found?
edata = edata %>%
  mutate(Cp.cap = (edata$TotalCoal / edata$Population))
edata = edata %>%
  mutate(region.i = (as.integer(edata$Region)))
str(edata)
head(edata)

pairs(edata)
boxplot(Cp.cap~region.i, data=edata, xlab="region", ylab="coal consumption per capita")

#3.c: assumptions
#normality:
qqnorm(edata$Cp.cap); qqline(edata$Cp.cap, col="Red") #data not normally distributed
shapiro.test(edata$Cp.cap) #not a normal distribution

#variance:
leveneTest(Cp.cap~Region, data=edata) #unequal variance

#3.d: test
kruskal.test(Cp.cap~region.i, data=edata)
#dunnTest...?

#4. What is the correlation between per capita coal use and per capita GDP? 
edata = edata %>%
  mutate(gdp.cap = (edata$TotalGDP / edata$Population))
plot(edata$Cp.cap, edata$gdp.cap)
cor(edata[,c("Cp.cap", "gdp.cap")])

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(hdata)
str(hdata)
pairs(hdata[,c("nox", "rad", "indus")])

#Q.5: correlation and multicolinearity
cor(hdata[,c("nox", "rad", "indus")], use="na.or.complete")
MR.Bhousing = lm(medv~nox+rad+indus, data=hdata) 
summary(MR.Bhousing)
vif(MR.Bhousing)

#Q.6:
plot(medv~nox, data=hdata)
abline(lm(medv~nox, data=hdata), col="red")
plot(medv~rad, data=hdata)
abline(lm(medv~rad, data=hdata), col="red")
plot(medv~indus, data=hdata)
abline(lm(medv~indus, data=hdata), col="red")

#Q.7: Multiple linear regression
MR.Bhousing = lm(medv~nox+rad+indus, data=hdata) 
summary(MR.Bhousing)

#Assumptions:
#Residual Independency (collinearity)
plot(MR.Bhousing)
plot(residuals(MR.Bhousing))
library(lmtest)
dwtest(MR.Bhousing, alternative=c("two.sided"))
# This indicates that there is some degree of autocorrelation within the residuals.

#homoscedasticity
plot(residuals(MR.Bhousing)~fitted(MR.Bhousing))
abline(lm(residuals(MR.Bhousing)~fitted(MR.Bhousing), col="red")) 
library(lmtest)
bptest(MR.Bhousing)
#The test indicates that the residuals are homoscedastic



#transformation with boxcox
bc = boxCox(hdata$medv~hdata$nox+hdata$rad+hdata$indus)
bc
(trans = bc$x[which.max(bc$y)])
#[1] -0.2626263
# re-run with transformation
H.new = lm(hdata$medv^trans ~ hdata$nox+hdata$rad+hdata$indus)
summary(H.new)
plot(H.new)
plot(H.new$residuals)
dwtest(H.new, alternative=c("two.sided"))
##DW = 0.38578, p-value < 2.2e-16
# This indicates that there is some degree of autocorrelation within the residuals.
