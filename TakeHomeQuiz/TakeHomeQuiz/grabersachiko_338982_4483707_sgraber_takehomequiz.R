setwd("M:/538 stats")
library(dplyr)
library(ggplot2)

# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

###1. Does per capita energy consumption differ depending on whether a state is found on the coast or not?
edata = mutate(edata,PerCapEnergy=TotalEnergy/Population)
Coast = filter(edata,Coast==1)
NoCoast = filter(edata,Coast==0)

#1b. 
hist(Coast$PerCapEnergy, border="red",main="Per Capita Energy Consumption",col=rgb(1,0,0,0.5),xlab="Per Capita Energy Consumption")
hist(NoCoast$PerCapEnergy,border="blue",col=rgb(0,0,1,0.5),add=TRUE)
abline(v=mean(Coast[,"PerCapEnergy"]),col="dark red")
abline(v=mean(NoCoast[,"PerCapEnergy"]),col="dark blue")
legend("topright",legend=c("Coast","Not Coast"),text.col=c("red","blue"))

#1c. test assumptions for 2-test
#data are continuous (yes! the y values are numeric.)
#randomly selected population (yes! since we have 51/51 states, we have selected the WHOLE population so I guess it's ok.)
#independent observations (yes! since each state is different/independent, observations do not effect each other)
#values are normal (yes!)
shapiro.test(edata$PerCapEnergy) #p<0.05 so we reject the null hypothesis that the data come from a normal distribution
#BUT sample size >30 so by CLT we can say values are normal *enough*
#check if variance is equal (yes!)
var.test(NoCoast$PerCapEnergy,Coast$PerCapEnergy) #p>0.5 so we cannot reject the null hypothesis that the variances are equal - so we assume variance is equal
#1d.
t.test(Coast$PerCapEnergy,NoCoast$PerCapEnergy,var.equal=TRUE)
#p=0.2, so we cannot reject the null hypothesis that there is no difference between the two sets



###2. does per capita coal consumption differ depending on whether a state is found on the coast or not?
edata = mutate(edata,PerCapCoal=TotalCoal/Population)
Coast = filter(edata,Coast==1)
NoCoast = filter(edata,Coast==0)
#2b.
hist(NoCoast$PerCapCoal,border="blue",main="Per Capita Coal consumption",col=rgb(0,0,1,0.5),xlab="Per capita coal consumption")
hist(Coast$PerCapCoal, border="red",col=rgb(1,0,0,0.5),add=TRUE)
abline(v=mean(Coast[,"PerCapCoal"]),col="red")
abline(v=mean(NoCoast[,"PerCapCoal"]),col="blue")
legend("topright",legend=c("Coast","Not Coast"),text.col=c("red","blue"))
#2c. this should be a t-test again because 2 categories and continuous y
#data are continuous - yes because numbers etc.
#randomly selected populations - yes as above
#independent observations - yes as above
#values are normal
shapiro.test(edata$PerCapCoal) #p<0.05 so we reject the null hypothesis that the data come from a normal distribution
#BUT sample size >30 so by CLT we can say values are normal *enough*
#check if variance is equal
var.test(NoCoast$PerCapCoal,Coast$PerCapCoal) #p<0.5 so we  reject the null hypothesis that the variances are equal - so we assume variance is unequal. 
#This is a problem. When variances are unequal, we use a special version of the t-test.

#2.d
t.test(Coast$PerCapCoal,NoCoast$PerCapCoal,var.equal=FALSE)
#p<0.05. We reject the null hypothesis that the difference in means = 0; in other words,
#we can say that the states on the coast have significantly different per capita coal consumption than states not on the coast.
#Our sample estimates show that states not on the coast have higher per capita coal consumption.



###3. Does per capita coal consumption differ depending on the region in which a state is found?
#need to use an anova 
#3b
ggplot(data=edata,aes(Region,PerCapCoal)) + geom_boxplot() + labs(title="Per Capital Coal Consumption",y="Per Capita Coal")
boxplot(PerCapCoal~Region, data=edata, xlab="Region", ylab="Per Capita Coal Consumption")
#3c
East=filter(edata,Region=="East")
East = edata %>% filter(Region=="East")
Midwest=filter(edata,Region=="Midwest")
South=filter(edata,Region=="South")
West=filter(edata,Region=="West")
var(East$PerCapCoal)
var(Midwest$PerCapCoal)
var(South$PerCapCoal)
var(West$PerCapCoal)
library(car)
leveneTest(PerCapCoal~Region,data=edata)

#3d
model = (aov(PerCapCoal~Region,data=edata))
summary(model)




###4. What is the correlation between per capita coal use and per capita GDP?
edata=mutate(edata,PerCapGDP=TotalGDP/Population)
cor(edata$PerCapCoal,edata$PerCapGDP)




###5. want to predict housing value prices medv
#5. Check for multi-collinearity
library(fmsb)
# all less than 10, so not multi-collinear. 
VIF(lm(crim~indus+ptratio,data=hdata))
VIF(lm(indus~crim+ptratio,data=hdata))
VIF(lm(ptratio~crim+indus,data=hdata))

cor(hdata$crim,hdata$indus)
cor(hdata$ptratio,hdata$indus)
cor(hdata$crim,hdata$ptratio) 

#6. plot the relationshpi between each independent variable and dependent variable.
plot(medv~crim,data=hdata,xlab="Per capita crime rate",ylab="housing value price",main="Housing price v. Crime rate",pch=19,cex=0.7)
plot(medv~indus,data=hdata,xlab="Proportion non-retail business land",ylab="housing value price",main="Housing price v.industry",pch=19,cex=0.7)
plot(medv~ptratio,data=hdata,xlab="Pupil-teacher ratio",ylab="housing value price",main="Housing price v. pupil-teacher ratio",pch=19,cex=0.7)

#7. 
#first, check assumptions
#normality
shapiro.test(hdata$crim)
shapiro.test(hdata$indus)
shapiro.test(hdata$ptratio)

qqnorm(hdata[,"crim"])
qqnorm(hdata[,"indus"])
qqnorm(hdata[,"ptratio"])

ggplot(data=hdata,mapping=aes(x=crim))+geom_histogram(aes(y=..density..))
hdata2=hdata%>%mutate(crim=log(crim))
ggplot(data=hdata2,mapping=aes(x=crim))+geom_histogram(aes(y=..density..))
#not perfet, but more normal

#autocorrelation?
resc=lm(medv~crim,data=hdata)$res
plot(resc~hdata$crim)
resi=lm(medv~indus,data=hdata)$res
plot(resi~hdata$indus)
resp=lm(medv~ptratio,data=hdata)$res
plot(resp~hdata$ptratio)

library(lmtest)
dwtest(hdata2$medv~hdata2$crim+hdata2$indus+hdata2$ptratio)

#homoscedasticity
bptest(hmodel)
bptest(lm(medv~crim,data=hdata))
bptest(lm(medv~indus,data=hdata2))
bptest(lm(medv~ptratio,data=hdata2))
#run model
hmodel=lm(medv~crim+indus+ptratio,data=hdata2)
summary(hmodel)

hmodel0=lm(medv~crim+indus+ptratio,data=hdata)
summary(hmodel0)

anova(hmodel,hmodel0)
#log transforming my data did nothing!

hmodel2=lm(medv~crim+indus+ptratio+indus:ptratio,data=hdata)
summary(hmodel2)
