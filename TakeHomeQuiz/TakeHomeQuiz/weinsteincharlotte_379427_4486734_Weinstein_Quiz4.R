# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

### Question 1:

#calculate per capita energy consumption for each state:
edata$PerCapitaEnergy = edata$TotalEnergy/edata$Population

#subset data for coastal & non-coastal states
onCoast = subset(edata, Coast==1)
notOnCoast = subset(edata, Coast==0)

#make histogram plot of per capita energy consumption in coastal & non-coastal states
hist(onCoast[,"PerCapitaEnergy"], breaks=15, col="light blue", xlim=c(0, 1), ylim=c(0,9), xlab="Per Capita Energy Consumption", main="")
abline(v=mean(onCoast[,"PerCapitaEnergy"]), col="blue")
par(new=TRUE)
hist(notOnCoast[,"PerCapitaEnergy"], breaks=15, col="light green", xlim=c(0,1), ylim=c(0,9), xlab="", main="")
abline(v=mean(notOnCoast[,"PerCapitaEnergy"]), col="dark green")
legend("topright", legend = c("Coastal mean", "Non-coastal mean"),text.col=c("blue", "dark green"))

#check assumptions of t-test:

#check for normality:
shapiro.test(onCoast[,"PerCapitaEnergy"])
#data is not normal, possibly due to the outliers at about 0.8

shapiro.test(notOnCoast[,"PerCapitaEnergy"])
#again, data is not normal

#check for equal variance:
var.test(onCoast[,"PerCapitaEnergy"], notOnCoast[,"PerCapitaEnergy"])

#conduct two-sample t-test:
t.test(onCoast[,"PerCapitaEnergy"], notOnCoast[,"PerCapitaEnergy"], paired=FALSE)


### Question 2

#calculate per capita coal consumption for each state:
edata$PerCapitaCoal = edata$TotalCoal/edata$Population

#re-do subset of edata so PerCapitaCoal is included:
onCoast = subset(edata, Coast==1)
notOnCoast = subset(edata, Coast==0)

#make histogram plot of per capita coal consumption in coastal & non-coastal states
hist(onCoast[,"PerCapitaCoal"], breaks=15, col="light blue", xlim=c(0, 0.85), ylim=c(0,11), xlab="Per Capita Coal Consumption", main="")
abline(v=mean(onCoast[,"PerCapitaCoal"]), col="blue")
par(new=TRUE)
hist(notOnCoast[,"PerCapitaCoal"], breaks=15, col="light green", xlim=c(0,0.85), ylim=c(0,11), xlab="", main="")
abline(v=mean(notOnCoast[,"PerCapitaCoal"]), col="dark green")
legend("topright", legend = c("Coastal mean", "Non-coastal mean"),text.col=c("blue", "dark green"))

#check assumptions of t-test:

#check for normality:
shapiro.test(onCoast[,"PerCapitaCoal"])
#data is not normally distributed - mode is to the left, with right tail

shapiro.test(notOnCoast[,"PerCapitaCoal"])
#again, data is not normal

#check for equal variance:
var.test(onCoast[,"PerCapitaCoal"], notOnCoast[,"PerCapitaCoal"])

#conduct two-sample t-test:
t.test(onCoast[,"PerCapitaCoal"], notOnCoast[,"PerCapitaCoal"], paired=FALSE)


### Question 3

#subset data by region:
East = subset(edata, Region=="East")
Midwest = subset(edata, Region=="Midwest")
South = subset(edata, Region=="South")
West = subset(edata, Region=="West")

#make box plot of per capita coal consumption by region
boxplot(PerCapitaCoal~Region, data=edata, xlab="Region", ylab="Per Capita Coal Consumption")

#check normality of data:

shapiro.test(East[,"PerCapitaCoal"])
#not normal

shapiro.test(Midwest[,"PerCapitaCoal"])
#not normal

shapiro.test(South[,"PerCapitaCoal"])
#not normal

shapiro.test(West[,"PerCapitaCoal"])
#not normal

#check for equal variances using Levene Test:
library(car)
leveneTest(PerCapitaCoal~Region, data=edata)

#Run the ANOVA test & view results
Region.aov = aov(PerCapitaCoal~Region, data=edata)
summary(Region.aov)

### Question 4

#calculate correlation between per capita coal consumption & per capita energy consumption
cor(edata$PerCapitaCoal, edata$PerCapitaEnergy)
cor.test(edata$PerCapitaCoal, edata$PerCapitaEnergy)


### Question 5

#calculate correlation between crim, rm, and age
df = data.frame(hdata$crim, hdata$rm, hdata$age)
cor(df)

#calculate VIF to check for multi-collinearity between crim, rm, and age
vif.medv = 1/(1 - summary(lm(medv~crim+rm+age, data=hdata))$r.squared)
vif.medv

### Question 6

#create plots of each variable with medv
plot(medv~crim, data=hdata)
plot(medv~rm, data=hdata)
plot(medv~age, data=hdata)

### Question 7

#create linear model
mod.medv = lm(medv~crim+rm+age, data=hdata)

#check for independence of errors
plot(residuals(mod.medv))
library(lmtest)
dwtest(mod.medv, alternative=c("two.sided"))

#check for homoscedasticity
plot(residuals(mod.medv)~fitted(mod.medv))
abline(lm(residuals(mod.medv)~fitted(mod.medv)), col="red")

#check for normality of error distribution
qqnorm(residuals(mod.medv))
qqline(residuals(mod.medv), col="red")
shapiro.test(residuals(mod.medv))

#look at results of model
summary(mod.medv)
