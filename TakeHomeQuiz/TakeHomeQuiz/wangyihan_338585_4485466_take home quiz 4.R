#Take home exam
#Question 1
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

install.packages("dplyr")
library(dplyr)
edata=edata %>%
  mutate(percapita=(TotalEnergy/Population))
head(edata)
boxplot(percapita~Coast, data=edata, xlab="coast",ylab="percapita(energy)")
shapiro.test(edata$percapita)
hist(edata$percapita)
edata$percapita[edata$percapita>0.8]=NA
head(edata$percapita)
shapiro.test(edata$percapita)
var.test(edata$percapita, edata$Coast)
percapita.Coast=subset(edata, Coast=="1")
percapita.Land=subset(edata, Coast=="0")
t.test(percapita.Coast[,"percapita"], percapita.Land[, "percapita"],var.equal=FALSE, paired=FALSE)

#Question 2
edata=edata %>%
  mutate(percapita=(TotalCoal/Population))
head(edata)
boxplot(percapita~Coast, data=edata, xlab="coast",ylab="percapita(coal)")
shapiro.test(edata$percapita)
hist(edata$percapita)
qqnorm(edata$percapita)
qqline(edata$percapita, col="red")
leveneTest(edata$percapita, edata$Coast)
kruskal.test(percapita~Coast, data=edata)

#Question 3
boxplot(percapita~Region, data=edata, xlab="Region",ylab="percapita(coal)")
shapiro.test(edata$percapita)
hist(edata$percapita)
leveneTest(edata$percapita, edata$Region)
aov=lm(percapita~Region, data=edata)
summary(aov)

#Question 4
edata=edata %>%
  mutate(perGDP=(TotalGDP/Population))
head(edata)
edata=edata %>%
  mutate(percapita=(TotalCoal/Population))
head(edata)
plot(perGDP~percapita, data=edata)
abline(lm(perGDP~percapita, data=edata), col="red")
cor(edata$perGDP, edata$percapita)
model=lm(percapita~perGDP, data=edata)
summary(model)
mod=lm(perGDP~percapita, data=edata)
summary(mod)res=residuals(mod)
head(res)
anova(mod)
plot(res)
install.packages("lmtest")
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red")
bptest(mod)
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
shapiro.test(residuals)

#Question 5
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

cor(hdata$age, hdata$crim)
cor(hdata$age, hdata$rm)
cor(hdata$rm, hdata$crim)
vif(lm(age~crim+rm, data=hdata))
vif(lm(crim~rm+age, data=hdata))
vif(lm(rm~age+crim, data=hdata))

#Question 6
plot(medv~age, data=hdata)
abline(lm(medv~age, data=hdata), col="red")
plot(hdata$crim, hdata$medv)
abline(lm(medv~crim, data=hdata), col="red")
plot(hdata$rm, hdata$medv)
abline(lm(medv~rm, data=hdata), col="red")

#Question 7 & 8
mod1=lm(medv~age+rm+crim, data=hdata)
summary(mod1)
mod2=lm(medv~age+rm, data=hdata)
summary(mod2)
mod3=lm(medv~rm+crim, data=hdata)
summary(mod3)
res1=residuals(mod1)
summary(res1)
shapiro.test(res1)
qqnorm(res1)
dwtest(mod1)
plot(residuals(mod1)~fitted(mod1))
abline(lm(residuals(mod1)~fitted(mod1)), col="red")
plot(res1)
mod4=lm(medv~age+rm*crim, data=hdata)
summary(mod4)

AIC(mod1)
AIC(mod4)
AIC(mod2)
AIC(mod3)
