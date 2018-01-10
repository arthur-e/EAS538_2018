#1
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
coast.states=subset(edata,Coast=="1")
nocoast.states=subset(edata,Coast=="0")
epercap.c=coast.states$TotalEnergy/coast.states$Population
epercap.nc=nocoast.states$TotalEnergy/nocoast.states$Population
hist(epercap.c, breaks=15, col="light blue", xlim=c(0, 1), ylim=c(0,10), xlab="Energy percapita", main="")
abline(v=mean(epercap.c), col="blue")
par(new=TRUE)
hist(epercap.nc, breaks=15, col="light green", xlim=c(0, 1), ylim=c(0,10), xlab="", main="")
abline(v=mean(epercap.nc), col="dark green")
legend("topright", legend = c("On Coast States mean", "Off Coast States mean"),text.col=c("blue", "dark green"))

shapiro.test(epercap.c) #violates normality but CLT
shapiro.test(epercap.nc) #violates normality but CLT
var.test(epercap.c, epercap.nc) #pvalue=.51, variances are equal
t.test(epercap.c, epercap.nc, paired=FALSE) #pvalue=.008

#2
coalpercap.c=coast.states$TotalCoal/coast.states$Population
coalpercap.nc=nocoast.states$TotalCoal/nocoast.states$Population
hist(coalpercap.c, breaks=5, col="light blue", xlim=c(0, .9), ylim=c(0,10), xlab="Coal percapita", main="")
abline(v=mean(coalpercap.c), col="blue")
par(new=TRUE)
hist(coalpercap.nc, breaks=15, col="light green", xlim=c(0, .9), ylim=c(0,10), xlab="", main="")
abline(v=mean(coalpercap.nc), col="dark green")
legend("topright", legend = c("On Coast States mean", "Off Coast States mean"),text.col=c("blue", "dark green"))

shapiro.test(coalpercap.c) #violates normality but CLT
shapiro.test(coalpercap.nc) #violates normality but CLT
var.test(coalpercap.c, coalpercap.nc) #violates variances are equal
t.test(coalpercap.c, coalpercap.nc, paired=FALSE) #significant p-value

#3
coalpercap=edata$TotalCoal/edata$Population
boxplot(coalpercap~Region, data=edata, ylim=c(0,1),xlab="Region", ylab="Coal percapita")
boxplot(coalpercap~Region, data=edata, ylim=c(0,.2),xlab="Region", ylab="Coal percapita")
shapiro.test(coalpercap) #violates normality but ok due to CLT
library(car)
leveneTest(coalpercap~Region, data=edata) #variances are equal, p-value .52
anova.coal=aov(coalpercap~Region, data=edata) 
summary(anova.coal) #region not significant
TukeyHSD(anova.coal) #to confirm lack of differences among regions

#4
GDPpercap=edata$TotalGDP/edata$Population
plot(coalpercap~GDPpercap, data=edata)
cor(coalpercap,GDPpercap)

#5
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
#covariates = crim, tax, dis
cor(hdata[, c("rm", "tax", "dis")], use="na.or.complete")
lm(medv~rm+tax+dis, data=hdata)
summary(lm(medv~rm+tax+dis, data=hdata)) #adjusted r^2=.584
vif.house = 1/(1 -.584)
vif.house

#6
plot(medv~rm, data=hdata, xlab="# rooms per dwelling")
summary(lm(medv~rm, data=hdata))
plot(medv~tax, data=hdata, xlab="property tax rate per 10K")
summary(lm(medv~tax, data=hdata))
plot(medv~dis, data=hdata, xlab="distance from employment centers")
summary(lm(medv~dis, data=hdata))

#7
mod.medv=lm(medv~rm+tax+dis, data=hdata)
plot(mod.medv)
summary(mod.medv)
plot(residuals(mod.medv))
#independence
library(lmtest)
dwtest(mod.medv, alternative=c("two.sided")) #residuals are autocorrelated

#homoscedasticity
plot(residuals(mod.medv)~fitted(mod.medv))
abline(lm(residuals(mod.medv)~fitted(mod.medv)), col="red")
bptest(mod.medv) #looks like there is a pattern, not homoscedastic, p=.2.36e-07

#normality
qqnorm(residuals(mod.medv))
qqline(residuals(mod.medv), col="red") 
shapiro.test(residuals(mod.medv)) #not normal, low p-value

#8
summary(mod.medv)
