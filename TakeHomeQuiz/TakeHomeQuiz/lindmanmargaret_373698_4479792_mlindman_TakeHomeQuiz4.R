# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
#1
edata["percapitaenergy"]<-edata$TotalEnergy/edata$Population
coast_en = subset (edata, Coast=="1")
nocoast_en = subset(edata, Coast=="0")

nocoast_vec = coast_en[, "percapitaenergy"]
coast_vec = nocoast_en[, "percapitaenergy"]

hist(nocoast_vec, breaks=15, col="light blue", xlim=c(0, 2), ylim=c(0,10), xlab="energy use", main="")
abline(v=mean(nocoast_vec), col="dark blue")
par(new=TRUE)
hist(coast_vec, breaks=15, col="light green", xlim=c(0, 2), ylim=c(0,10), xlab="", main="")
abline(v=mean(coast_vec), col="dark green")
legend("topright", legend = c("No coast mean", "Coast mean"),text.col=c("blue", "dark green"))

t.test(nocoast_vec,coast_vec, paired=FALSE)

# test for normality
shapiro.test(nocoast_vec)
shapiro.test(coast_vec)
#does not pass test for normality; however sample size is sufficiently large (n>30)
wilcox.test(nocoast_vec,coast_vec, data=edata)

#test for equal variance
var.test(nocoast_vec, coast_vec)
#Passes test for equal variance

#2
edata["percapitacoal"]<-edata$TotalCoal/edata$Population
hist(edata$percapitaenergy, breaks=20)
nocoast_vec1 = coast_en[, "percapitacoal"]
coast_vec1 = nocoast_en[, "percapitacoal"]

hist(nocoast_vec1, breaks=15, col="orange", xlim=c(0, 1.5), ylim=c(0,10), xlab="coal use", main="")
abline(v=mean(nocoast_vec1), col="dark blue")
par(new=TRUE)
hist(coast_vec1, breaks=15, col="purple", xlim=c(0, 1.5), ylim=c(0,10), xlab="", main="")
abline(v=mean(coast_vec1), col="orange")
legend("topright", legend = c("No coast mean", "Coast mean"),text.col=c("purple", "orange"))

# test for normality
shapiro.test(nocoast_vec1)
shapiro.test(coast_vec1)
#Does not pass test for normality
wilcox.test(nocoast_vec1, coast_vec1, data=edata)

#test for equal variance
var.test(nocoast_vec1, coast_vec1)
#Does not pass test for equal variance

t.test(nocoast_vec1,coast_vec1, paired=FALSE)

#3

E = subset (edata, Region=="East")
W = subset (edata, Region=="West")
MW = subset (edata, Region=="Midwest")
S = subset (edata, Region=="South")


east_coal = E[, "percapitacoal"]
west_coal = W[, "percapitacoal"]
midwest_coal = MW[, "percapitacoal"]
south_coal = S[, "percapitacoal"]
install.packages("car")
library(car)
levene.test(edata$percapitacoal, edata$Region)
#P-value is greater than 0.05, therefore we do not have sufficient evidence to reject the null hypothesis that the variances are not significantly different.
shapiro.test(east_coal)
shapiro.test(west_coal)
shapiro.test(midwest_coal)
shapiro.test(south_coal)
#P-values less than 0.05, therefore we reject the null hypothesis that data are normal
#boxplot()
boxplot(percapitacoal~Region, data=edata, xlab="Region", ylab="per capita coal mean (effect size)")
#variances are not equal and data is non normal: ANOVA

kruskal.test(percapitacoal~Region, data = edata)


aov<-aov(percapitacoal~Region, data=edata)
summary(aov)

#4
edata["percapitaGDP"]<-edata$TotalGDP/edata$Population
cor.test(edata$percapitaGDP, edata$percapitacoal)

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
#5

housing<-lm(medv~crim + ptratio + rm, data=hdata)
summary(housing)
cor.test(hdata$crim, hdata$ptratio)
cor.test(hdata$ptratio, hdata$rm)

vif.house = 1/(1 - summary(lm(medv ~ crim + ptratio + rm, data=hdata))$r.squared)
vif.house

#6

plot(medv~crim, data=hdata)
abline(lm(medv~crim, data=hdata), col="red")

plot(medv~ptratio, data=hdata)
abline(lm(medv~ptratio, data=hdata), col="red")

plot(medv~rm, data=hdata)
abline(lm(medv~rm, data=hdata), col="red")


#7
housing<-lm(medv~crim + ptratio + rm, data=hdata)
install.packages(lmtest)
library(lmtest)
bptest(housing)
shapiro.test(residuals(housing))
#8
housing<-lm(medv~crim + ptratio + rm, data=hdata)
housing2= lm(medv~ crim*rm, data=hdata)
summary(housing2)
#9
summary(log10(hdata$medv))
summary(sqrt(hdata$medv))

