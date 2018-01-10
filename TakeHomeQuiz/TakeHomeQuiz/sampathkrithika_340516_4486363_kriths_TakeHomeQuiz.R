# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", sep=",", fill=TRUE, header=TRUE)
edata
###Question 1
pc.energy<-edata$TotalEnergy/edata$Population
hist(pc.energy)
#data is not normally distributed.
t.test(edata$Coast, pc.energy)
#result is not significant. 

###Question 2
pc.coal<-edata$TotalCoal/edata$Population
hist(pc.coal)
#data is not normally distributed. log transformation of the data to get a normalized distribution. 
log.pcc<-log(pc.coal)
hist(log.pcc)
t.test(edata$Coast, pc.coal)

###Question 3
hist(pc.coal)
#data is not normally distributed
x<- aov(pc.coal~edata$Region)
summary(x)
model.tables(x)
#result is not significant in the 95% confidence level. 

###Question 4
pc.GDP<-edata$TotalGDP/edata$Population
cor(pc.coal,pc.GDP)
plot(pc.coal,pc.GDP)
# weak positive correlation. 


# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", sep=",", fill=TRUE, header=TRUE)
hdata
###Question 5
cor(hdata [, c("rm", "age","crim")])
vif.rm=1/(1-summary(lm(rm~age+crim, data=hdata))$r.squared)
vif.age=1/(1-summary(lm(age~rm+crim, data=hdata))$r.squared)
vif.crim=1/(1-summary(lm(crim~age+rm, data=hdata))$r.squared)
vif.crim
vif.age
vif.rm
#no strong correlation between the variables- no multi- collinearity issues

### Question 6
plot(medv~rm, data= hdata)
abline(lm(medv~rm, data= hdata ), col="red")
plot(medv~age, data= hdata)
abline(lm(medv~age, data= hdata), col="blue")
plot(medv~crim, data=hdata)
abline(lm(medv~crim, data= hdata), col="green")

###Question 7
hist(hdata$rm)
hist(hdata$age)
hist(hdata$crim)
hist(hdata$medv)
hist(log(hdata$age))
hist(log(hdata$crim))
hist(log(hdata$medv))
mod<-lm(log(medv)~rm+log(age)+log(crim), data=hdata)
summary(mod)
mod1<-lm(log(medv)~rm*nox+log(age)+log(crim), data=hdata)
summary(mod1)