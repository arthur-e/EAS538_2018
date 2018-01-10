# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

percapitaenergy = edata$TotalEnergy / edata$Population
plot(percapitaenergy~edata$Coast)
t.test(percapitaenergy~edata$Coast)

percapitacoal = edata$TotalCoal / edata$Population
plot(percapitacoal~edata$Coast)
t.test(percapitacoal~edata$Coast)

plot(percapitacoal~edata$Region)
region = edata$Region
leveneTest(percapitacoal~region)
aov(percapitacoal~region)
summary(aov(percapitacoal~region))
lm(percapitacoal~region)

percapitaGDP = edata$TotalGDP / edata$Population
plot(percapitacoal, percapitaGDP)
summary(lm(percapitacoal~percapitaGDP))

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

pairs(hdata[,c("crim", "nox", "rad")])
cor(hdata[,c("crim", "nox", "rad")])
mod = lm(medv~crim+nox+rad, data = hdata)
summary(mod)
1/(1 - summary(lm(medv~crim+nox+rad, data = hdata))$r.squared)
vif(mod)

plot(mod)
pairs(hdata[,c("crim", "nox", "rad", "medv")])

library(lmtest)
dwtest(mod, alternative=c("two.sided"))
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red")
bptest(mod)
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
shapiro.test(residuals(mod))

res = residuals(mod)
RSS = sum(res^2)
SSY = deviance(lm(hdata$medv~1))
R2 = 1 - RSS/SSY
R2
anova(mod)