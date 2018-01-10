## Part 1
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv",
sep=",", fill=TRUE, header=TRUE)
energypercapita = edata$TotalEnergy/edata$Population
coalpercapita = edata$TotalCoal/edata$Population
t.test(edata$Coast, energypercapita)
gdppercapita = edata$TotalGDP/edata$Population
cor(coalpercapita, gdppercapita)
## correlation is 0.0359
cor.test(coalpercapita, gdppercapita)
## p value is 0.802, t= 0.252

t.test(edata$Coast, energypercapita)
t.test(edata$Coast, coalpercapita)
t.test(edata$Region, coalpercapita)


boxplot(edata$Coast, energypercapita)
boxplot(edata$Coast, coalpercapita)
plot(edata$Region, coalpercapita)


## Part 2
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv",
sep=",", fill=TRUE, header=TRUE)
cor(hdata$rm, hdata$ptratio) ## correlation coefficients
cor(hdata$nox, hdata$ptratio)
cor(hdata$nox, hdata$rm)
res = lm(medv~ rm+ptratio+nox, data=hdata) ## linear model
summary(res)
VIF(lm(ptratio ~ nox + rm, data=hdata))
VIF(lm(nox ~ ptratio + rm, data=hdata))
VIF(lm(rm ~ nox + ptratio, data=hdata))

## Questions 6 and 7 plots

modelptratio= lm(medv~ ptratio, data=hdata)
summary(modelptratio)
plot(modelptratio)

modelrm= lm(medv~ rm, data=hdata)
summary(modelrm)
plot(modelrm)

modelnox= lm(medv~ nox, data=hdata)
summary(modelnox)
plot(modelnox)


