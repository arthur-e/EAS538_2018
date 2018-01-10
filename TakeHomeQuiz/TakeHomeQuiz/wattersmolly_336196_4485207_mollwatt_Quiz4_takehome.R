# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

####Question 1
###Part A
#make new column that's total energy divided by population to get per capita energy consumption

PerCapita=edata$TotalEnergy/edata$Population
edata2=cbind(edata, PerCapita)

Coast2=subset(edata2,Coast==1)
NoCoast2=subset(edata2,Coast==0)

###Part B: Make a plot
boxplot(Coast2[,"PerCapita"],NoCoast2[,"PerCapita"], names=c("Coastal","Not Coastal"),xlab="Coast", ylab="Per Capita Energy Consumption")

###Part C: two sample t-test
##Test assumptions
#Equal Variance
var.test(Coast2[,"PerCapita"], NoCoast2[,"PerCapita"])
#null would be that the ratio of variance is equal to 1, which means they're equal
#p-value is 0.5098, so we cannot reject the null, so our assumption that variance is equal is met

#Normal Distribution
shapiro.test(Coast2[,"PerCapita"])
shapiro.test(NoCoast2[,"PerCapita"])
#Fail--p values are small which means we reject the null of normal distribution

t.test(Coast2[,"PerCapita"], NoCoast2[,"PerCapita"], paired=FALSE)

####Question 2---------------------------------------
###Part A
#make new column that's total coal divided by population to get per capita energy consumption

PerCapCoal=edata$TotalCoal/edata$Population
edata3=cbind(edata2, PerCapCoal)

Coast3=subset(edata3,Coast==1)
NoCoast3=subset(edata3,Coast==0)

###Part B: Make a plot
boxplot(Coast3[,"PerCapCoal"],NoCoast3[,"PerCapCoal"], names=c("Coastal","Not Coastal"),xlab="Coast", ylab="Per Capita Coal Consumption")

###Part C: two sample t-test
##Test assumptions
#Equal Variance
var.test(Coast3[,"PerCapCoal"], NoCoast3[,"PerCapCoal"])
#fail

#Normal Distribution
shapiro.test(Coast3[,"PerCapCoal"])
shapiro.test(NoCoast3[,"PerCapCoal"])
#Fail--p values are small which means we reject the null of normal distribution

t.test(Coast3[,"PerCapCoal"], NoCoast3[,"PerCapCoal"], paired=FALSE)

####Question 3--------------------------------------
###Part A
#subset by region

East=subset(edata3,Region=="East")
West=subset(edata3,Region=="West")
Midwest=subset(edata3,Region=="Midwest")
South=subset(edata3,Region=="South")


###Part B: Make a plot
boxplot(East[,"PerCapCoal"],West[,"PerCapCoal"],Midwest[,"PerCapCoal"],South[,"PerCapCoal"],names=c("East","West","Midwest","South"),xlab="Region", ylab="Per Capita Coal Consumption")

###Part C: two sample t-test
##Test assumptions
#Equal Variance

library(Rcmdr)
library(car)
leveneTest(PerCapCoal~Region,data=edata3)

#Normal Distribution
shapiro.test(East[,"PerCapCoal"])
shapiro.test(West[,"PerCapCoal"])
shapiro.test(Midwest[,"PerCapCoal"])
shapiro.test(South[,"PerCapCoal"])

Region.aov=aov(PerCapCoal~Region, data=edata3)
Region.aov

###Question 4------------------------------------
##correlation between per capita coal and per capita GDP

PerCapGDP=edata$TotalGDP/edata$Population
edata4=cbind(edata3, PerCapGDP)

cor(PerCapGDP, PerCapCoal)
#small positive linear correlation (0.036)

#######------------------------------------------------
###Question 5----------------------------------------

cor(hdata[,c("crim", "rm", "ptratio")], use="na.or.complete")

#VIF (variance inflation factor) is another way to calculate magnitude of collinearity
vif.crim = 1/(1 - summary(lm(crim~rm+ptratio, data=hdata))$r.squared)
vif.crim

vif.rm = 1/(1 - summary(lm(rm~crim+ptratio, data=hdata))$r.squared)
vif.rm

vif.ptratio = 1/(1 - summary(lm(ptratio~rm+crim, data=hdata))$r.squared)
vif.ptratio


###Question 6----------------

plot(medv~crim, data=hdata)
abline(lm(medv~crim, data=hdata), col="red")


plot(medv~rm, data=hdata)
abline(lm(medv~rm, data=hdata), col="red")

plot(medv~ptratio, data=hdata)
abline(lm(medv~ptratio, data=hdata), col="red")

###Question 7-----------------------------
modmedv=lm(formula=medv~crim+rm+ptratio,data=hdata)
summary(modmedv)

#test assumption of autocorrelation
plot(residuals(modmedv))
library(lmtest)
dwtest(modmedv, alternative=c("two.sided"))

#test assumption of homoscedasticity
plot(residuals(modmedv)~fitted(modmedv))
abline(lm(residuals(modmedv)~fitted(modmedv)), col="red")

library(lmtest)
bptest(modmedv)

#normality
shapiro.test(residuals(modmedv))

qqnorm(residuals(modmedv))
qqline(residuals(modmedv), col="red")

###Question 8---------------------------------






