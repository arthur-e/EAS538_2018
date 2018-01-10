# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

Coast = subset(edata, Coast== "1")
head(Coast)
Coastpercapita= Coast$TotalEnergy/Coast$Population
head(Coastpercapita)

NoCoast= subset(edata, Coast == '0')
head(NoCoast)
NoCoastpercapita= NoCoast$TotalEnergy/NoCoast$Population
head(NoCoastpercapita)

boxplot(Coastpercapita, NoCoastpercapita, data=edata, xlab="Coast vs No Coast", ylab="Per capita Energy Consumption", main="Per Capita Energy Consumption Based on Proximity to a Coast")

var.test(NoCoastpercapita, Coastpercapita, data=edata)
shapiro.test(NoCoastpercapita)
shapiro.test(Coastpercapita)

mod1 = t.test(Coastpercapita, NoCoastpercapita, paired=FALSE)
mod1

#################
Coastcoal= Coast$TotalCoal/Coast$Population
NoCoastCoal= NoCoast$TotalCoal/NoCoast$Population

boxplot(Coastcoal, NoCoastCoal, data=edata, xlab="Coast vs No Coast", ylab="Per capita Coal Consumption", main="Per Capita Coal Consumption Based on Proximity to a Coast")

shapiro.test(NoCoastCoal)
shapiro.test(Coastcoal)
var.test(NoCoastCoal, Coastcoal)

t.test(Coastcoal, NoCoastCoal, paired=FALSE)

#########################################################

percapita=edata$TotalCoal/edata$Population
regiondata = cbind(percapita, edata$Region)
head(regiondata)

Regionaov= data.frame(regiondata)

boxplot(percapita~V2, data=regiondata, xlab="Region", ylab="Total Coal Consumption", main="Per Capital Coal Consumption Based on State Region")

leveneTest(percapita, V2, data=regiondata)
shapiro.test(percaipta)
shapiro.test(V2)

kruskal.test(percapita~V2, data=Regionaov)

##########

Coal = edata$TotalCoal/edata$Population
GDP = edata$TotalGDP/edata$Population
datacoalgdp= cbind(Coal, GDP)
Datacoaldpd2= data.frame(datacoalgdp)

cor(Datacoaldpd2[,c("Coal", "GDP")], use="na.or.complete")

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
cor(hdata[,c("crim", "nox", "lstat")], use="na.or.complete")

vif.rad = 1/ (1-summary(lm(medv~crim+nox+lstat, dat=hdata))$r.squared)
vif.rad

pairs(hdata[,c("crim", "nox", "lstat")])
plot(medv~crim, data=hdata, main="Collinearity of Crime and Housing Value Prices")
plot(medv~nox, data=hdata, main="Collinearity of Nitric Oxide Concentration and Housing Value Prices")
plot(medv~lstat, data=hdata, main="Collinearity of Lower Status of Population and Housing Value Prices")

mod= lm(medv~crim+nox+lstat, dat=hdata)
summary(mod)

install.packages("lmtest")
library("lmtest")
bptest(mod)
dwtest(mod)
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")

mod= lm(medv~crim+nox+lstat, dat=hdata)
summary(mod)