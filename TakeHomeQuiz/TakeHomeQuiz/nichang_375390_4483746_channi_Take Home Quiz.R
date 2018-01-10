setwd("D:/UM/SNRE/Winter 2017/NRE 538 Statistics/Week 11 Take Home Quiz")
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
energypercapita=edata$TotalEnergy/edata$Population
coalpercapita=edata$TotalCoal/edata$Population
GDPpercapita=edata$TotalGDP/edata$Population

edata1=cbind(edata,energypercapita,coalpercapita,GDPpercapita)

#1 Does per capita energy consumption differ depending on whether a state is found on the coast?
#a Hypothesis
#b visual plot
boxplot(energypercapita~Coast,data=edata1,xlab="Coast",ylab="energy per capita")

#c Check assumptions
coastal=subset(edata1,Coast==1)
hist(coastal$energypercapita)
qqnorm(coastal$energypercapita)
qqline(coastal$energypercapita,col="red")
shapiro.test(coastal$energypercapita)

inland=subset(edata1,Coast==0)
hist(inland$energypercapita)
qqnorm(inland$energypercapita)
qqline(inland$energypercapita,col="red")
shapiro.test(inland$energypercapita)

var.test(coastal$energypercapita,inland$energypercapita)

#d Run and interpret test
t.test(coastal$energypercapita,inland$energypercapita)

#2 Does per capita coal consumption differ depending on whether a state is found on the coast?
#a Hypothesis
#b visual plot
boxplot(coalpercapita~Coast,data=edata1,xlab="Coast",ylab="Coal per Capita")
#c Check Assumption
inlandcoal=subset(edata1,Coast==0)
hist(inlandcoal$coalpercapita)
qqnorm(inlandcoal$coalpercapita)
qqline(inlandcoal$coalpercapita,col="red")
shapiro.test(inlandcoal$coalpercapita)

coastcoal=subset(edata1,Coast==1)
hist(coastcoal$coalpercapita)
qqnorm(coastcoal$coalpercapita)
qqline(coastcoal$coalpercapita,col="red")
shapiro.test(coastcoal$coalpercapita)

var.test(inlandcoal$coalpercapita,coastcoal$coalpercapita)
#d Run and interpret
t.test(inlandcoal$coalpercapita,coastcoal$coalpercapita)

#3 Does per capita coal consumption differ depending on the region in which a state is found?
#a Hypothesis
#b visual plot
boxplot(coalpercapita~Region,data=edata1,xlab="Region",ylab="Coal per Capita")
#c Check assumption
coaleast=subset(edata1,Region=="East")
coalmidwest=subset(edata1,Region=="Midwest")
coalsouth=subset(edata1,Region=="South")
coalwest=subset(edata1,Region=="West")

hist(coaleast$coalpercapita)
shapiro.test(coaleast$coalpercapita)

hist(coalmidwest$coalpercapita)
shapiro.test(coalmidwest$coalpercapita)

hist(coalsouth$coalpercapita)
shapiro.test(coalsouth$coalpercapita)

hist(coalwest$coalpercapita)
shapiro.test(coalwest$coalpercapita)

library(car)
leveneTest(coalpercapita~Region,data=edata1)

#d Run and interpret
modcoal=aov(coalpercapita~Region,data=edata1)
summary(modcoal)

#4 What is the correlation between per capita coal use and per capita GDP? 
#Does this seem like a strong correlation to you? Why or why not?
plot(edata1$GDPpercapita,edata1$coalpercapita)
abline(lm(coalpercapita~GDPpercapita,data=edata1), col="red")
cor(edata1$GDPpercapita,edata1$coalpercapita)

####5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

#5 check if these variables are highly correlated with one another to make sure you do not run into problems of multi-collinearity
pairs(hdata[,c(8,9,13)])
vifdis=1/(1-(summary(lm(dis~rad+lstat, dat=hdata))$r.squared))
vifdis

vifrad=1/(1-(summary(lm(rad~dis+lstat, dat=hdata))$r.squared))
vifrad

viflstat=1/(1-summary(lm(lstat~dis+rad, dat=hdata))$r.squared)
viflstat

cor(hdata[,c(8,9,13,14)])

#6 plot
plot(hdata$dis,hdata$medv)
abline(lm(medv~dis, data=hdata), col="red")
plot(hdata$rad,hdata$medv)
abline(lm(medv~rad, data=hdata), col="red")
plot(hdata$lstat,hdata$medv)
abline(lm(medv~lstat, data=hdata), col="red")

#7 run model and check assumptions
mod=lm(medv~dis+rad+lstat,data=hdata)
summary(mod)

#Independent Error
plot(residuals(mod))
library(lmtest)
dwtest(mod,alternative = c("two.sided"))

#Homoscedasticity
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red")
bptest(mod)

#Normally distributed error
qqnorm(residuals(mod))
qqline(residuals(mod),col="red")

shapiro.test(residuals(mod))

mod1=lm(medv~dis+rad+lstat+dis*rad*lstat,data=hdata)
summary(mod1)
