#Erin Barton
#28 March, 2017
#R Script for Take Home Quiz

# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

#Question 1: 
edata$percapitaE <- edata$TotalEnergy/edata$Population
attach(edata)

#create plot of data
boxplot(percapitaE~Coast, data=edata, main="Per Capita Energy Consumption by Region", xlab="Coastal (1), Interior (0)",ylab="Per Capita Energy Consumption")
Coastal <- subset(edata, Coast=="1")
Interior <- subset(edata, Coast=="0")

#test assumptions of two-sample t-test
#variance
var.test(Coastal$percapitaE, Interior$percapitaE)
#normality
shapiro.test(edata$percapitaE)
shapiro.test(Coastal$percapitaE)
qqnorm(Coastal$percapitaE)
qqline(Coastal$percapitaE,col="red")

shapiro.test(Interior$percapitaE)
qqnorm(Interior$percapitaE)
qqline(Interior$percapitaE,col="red")

#transformation to correct for nonnormality
edata$logE <- log(edata$percapitaE)
shapiro.test(edata$logE)

Coastal$logE <- log(Coastal$percapitaE)
shapiro.test(Coastal$logE)
qqnorm(Coastal$logE)
qqline(Coastal$logE,col="red")

Coastal$sqrtE <- sqrt(Coastal$percapitaE)
shapiro.test(Coastal$sqrtE)
qqnorm(Coastal$sqrtE)
qqline(Coastal$sqrtE,col="red")

Interior$logE <- log(Interior$percapitaE)
shapiro.test(Interior$logE)
qqnorm(Interior$logE)
qqline(Interior$logE,col="red")

#wilcox test
t.test(percapitaE~Coast, data=edata)
wilcox.test(percapitaE~Coast, data=edata)


#Question 2: 
attach(edata)
edata$percapitaCoal <- TotalCoal/Population

#visual plot of data
boxplot(percapitaCoal~Coast, data=edata, main="Per Capita Coal Consumption by Region", xlab="Coastal (1), Interior (0)",ylab="Per Capita Coal Consumption")

#test for variance
detach(edata)
attach(Coastal)
Coastal$percapitaCoal <- TotalCoal/Population
detach(Coastal)
attach(Interior)
Interior$percapitaCoal <- TotalCoal/Population
detach(Interior)

var.test(Interior$percapitaCoal, Coastal$percapitaCoal)

#test for normality
shapiro.test(Coastal$percapitaCoal)
qqnorm(Coastal$percapitaCoal)
qqline(Coastal$percapitaCoal,col="red")

shapiro.test(Interior$percapitaCoal)
qqnorm(Interior$percapitaCoal)
qqline(Interior$percapitaCoal,col="red")

#t.test
t.test(Coastal$percapitaCoal,Interior$percapitaCoal,paired=FALSE)


#Question 3
attach(edata)

#visual of data
boxplot(percapitaCoal~Region, data=edata, main="Per Capita Coal Consumption by Region", xlab="Region",ylab="Per Capita Coal Consumption")

#test normality
shapiro.test(edata$percapitaCoal)
qqnorm(edata$percapitaCoal)
qqline(edata$percapitaCoal,col="red")

#test variance
install.packages("car")
library(car)
leveneTest(percapitaCoal~Region,data=edata)

#run ANOVA
mod <- lm(percapitaCoal~Region, data=edata)
summary(mod)
detach(edata)

#Question 4
attach(edata)
edata$percapitaGDP <- TotalGDP/Population
cor(edata$percapitaGDP, edata$percapitaCoal)
plot(edata$percapitaGDP, edata$percapitaCoal)
detach(edata)


#Question 5
# covariates: RM, NOX, PTRATIO
attach(hdata)
cor(hdata[, c("rm", "nox", "ptratio")], use="na.or.complete")
vif.hdata = 1/(1 - summary(lm(medv~rm+nox+ptratio, dat=hdata))$r.squared)
vif.hdata

#plot data
pairs(hdata[, c("medv","rm", "nox", "ptratio")])
plot(medv~rm, data=hdata)
plot(medv~nox, data=hdata)
plot(medv~ptratio, data=hdata)

#run model
mod1 <- lm(medv~rm+nox+ptratio, data=hdata)
summary(mod1)

#check for outliers
cooksd <- cooks.distance(mod1)
cooksd
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 4*mean(cooksd, na.rm=T), col="red") 
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#check linear relationship
pairs(hdata[, c("medv","rm", "nox", "ptratio")])
plot(medv~rm, data=hdata)
abline(lm(medv~rm, data=airquality), col="red")
plot(medv~nox, data=hdata)
abline(lm(medv~nox, data=airquality), col="red")
plot(medv~ptratio, data=hdata)
abline(lm(medv~ptratio, data=airquality), col="red")

#check independence of errors
plot(residuals(mod1))
library(lmtest)
dwtest(mod1, alternative=c("two.sided"))

#check homoscedascticity
plot(residuals(mod1)~fitted(mod1))
abline(lm(residuals(mod1)~fitted(mod1)), col="red")
library(lmtest)
bptest(mod1)

#check normality of errors
qqnorm(residuals(mod1))
qqline(residuals(mod1), col="red")
shapiro.test(residuals(mod1))


