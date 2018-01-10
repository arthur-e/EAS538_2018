#Quiz4
#Katie Grantham

#Questions 1-4

#Question 1
#1A Null and Alternative Hypothesis:
  #Null: Per capita energy consumption does not differ between states found on the coast and states not found on the coast
  #Alternative: Per capitat energy consumption is higher for states found on the coast than states not found on the coast. 

#1b
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
sep=",", fill=TRUE, header=TRUE)
head(edata)

edata$percapitaenergy= edata$TotalEnergy /edata$Population

boxplot(percapitaenergy~Coast, data=edata, main="Per Capita Energy Consumption by Coast Location", ylab="Per Capita Energy Consumption", xlab="Coast Location")
qqnorm(edata$percapitaenergy); qqline(edata$percapitaenergy, col="Red")
shapiro.test(On.Coast$percapitaenergy)
shapiro.test(Off.Coast$percapitaenergy)
var.test(Off.Coast$percapitaenergy, On.Coast$percapitaenergy)


On.Coast= subset(edata, Coast== "1")
Off.Coast= subset(edata, Coast== "0")
t.test(On.Coast$percapitaenergy, Off.Coast$percapitaenergy, paired=FALSE)


#Question 2
  #1. Null and Alternative Hypothesis
    #Null: Per Captita Coal consumption does not differ between states found on the coast and states not found on the coast. 
    #Alternative: Per capita coal consumption is higher in states that are on the coast than states that are not on the coast. 

edata$percapitacoal= edata$TotalCoal / edata$Population
boxplot(percapitacoal~Coast, data = edata, main="Per Captita Coal Consumption by Coast Location", ylab="Per Capita Coal Consumption", xlab="Coast Location")
qqnorm(On.Coast$percapitacoal); qqline(On.Coast$percapitacoal, col="Red")
qqnorm(Off.Coast$percapitacoal); qqline(Off.Coast$percapitacoal, col="Red")
shapiro.test(On.Coast$percapitacoal)
shapiro.test(Off.Coast$percapitacoal)
var.test(On.Coast$percapitacoal, Off.Coast$percapitacoal)
t.test(On.Coast$percapitacoal, Off.Coast$percapitacoal)

#Question 3
#1

boxplot(percapitacoal~Region, data=edata, main="Per Capita Coal Consumption by Region", xlab="Region", ylab="Per Capita Coal Consumption")
qqnorm(edata$percapitacoal); qqline(edata$percapitacoal, col="Red")
shapiro.test(edata$percapitacoal)
install.packages("car")
library(car)
leveneTest(percapitacoal~Region, data=edata)
mod1=aov(percapitacoal~Region, data=edata)
summary(mod1)

kruskal.test(percapitacoal~Region, data = edata)

#Question 4
edata$percapitaGDP= edata$TotalGDP/ edata$Population
cor(edata$percapitacoal, edata$percapitaGDP)

#Question 5
pairs(~crim + age + ptratio, data = hdata)
cor(hdata[, c(1,7,11)])
vif(lm(medv~crim+age+ptratio, data=hdata))

#Question 6
plot(medv~crim, data=hdata)
plot(medv~age, data = hdata)
plot(medv~ptratio, data = hdata)

#Question 7
mod2= lm(medv~crim+age+ptratio, data=hdata)
summary(mod2)

#Testing Assumptions
plot(residuals(mod2))
m3=residuals(mod2)
summary(resid)

library(lmtest)
dwtest(mod2, alternative = c("two.sided"))
bptest(mod2)
qqnorm(residuals(mod2)); qqline(residuals(mod2), col="red")
shapiro.test(resid)
