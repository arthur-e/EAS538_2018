# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(edata)
coast <- subset(edata, Coast=="1")
head(coast)
pcapcoast = coast$TotalEnergy/coast$Population
plot(pcapcoast)
notcoast= subset(edata, Coast=="0")
pcapnotcoast= notcoast$TotalEnergy/notcoast$Population
qqnorm(pcapcoast)
qqnorm(pcapnotcoast)
shapiro.test(pcapcoast)
shapiro.test(pcapnotcoast)
var.test(pcapcoast, pcapnotcoast)
boxplot(pcapcoast, pcapnotcoast)
install.packages("outliers")
pcoast2=rm.outlier(pcapcoast, fill= FALSE, median= FALSE, opposite= FALSE)
pnotcoast2=rm.outlier(pcapnotcoast, fill= FALSE, median= FALSE, opposite= FALSE)
shapiro.test(pcoast2)
boxplot(pcoast2, pnotcoast2)
var.test(pcoast2, pnotcoast2)
t.test(pcapcoast, pcapnotcoast, var.equal = FALSE)
pcoalcoast = coast$TotalCoal/coast$Population
pcoalnotcoast= notcoast$TotalCoal/notcoast$Population
boxplot(pcoalcoast, pcoalnotcoast)
qqnorm(pcoalcoast)
qqnorm(pcoalnotcoast)
shapiro.test(pcoalcoast)
shapiro.test(pcoalnotcoast)
var.test(pcoalcoast, pcoalnotcoast)
t.test(pcoalcoast, pcoalnotcoast)
plot(edata$Region, edata$TotalCoal)
shapiro.test(edata$TotalCoal)
qqnorm(edata$TotalCoal)
?aov
coaltest= aov(TotalCoal~Region, data=edata)
summary(coaltest)
TukeyHSD(coaltest)
pcapcoal= edata$TotalCoal/edata$Population
pcapGDP= edata$TotalGDP/edata$Population
cor(pcapcoal, pcapGDP)
cor.test(pcapcoal, pcapGDP)


# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
cor(hdata[,c(1:14)], use="na.or.complete")
?vif
vif(lm(medv~crim+nox+rm, data=hdata))
plot(hdata$crim, hdata$medv)
plot(hdata$nox, hdata$medv)
plot(hdata$rm, hdata$medv)
housemod=lm(medv~crim+nox+rm, data=hdata)
summary(housemod)             
plot(housemod)
qnorm(housemod)

