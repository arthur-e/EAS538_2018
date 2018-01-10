# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
###
###Question 1
edata$per_capita_Energy_Consumption = edata$TotalEnergy / edata$Population
coast1 = subset(edata, Coast == "1")
no_coast1 = subset(edata, Coast == "0")
coast = coast1[, 8]
no_coast = no_coast1[, 8]
qqnorm(edata[, 8])#testing for normaility 
qqline(edata[, 8], col="red")
shapiro.test(edata[, 8])
var.test(coast, no_coast)#testing equal variances
boxplot(coast, no_coast, names = c("Coast", "No Coast"), ylab = "Per Capita Energy Consumption")
t.test(coast, no_coast, alternative = "two.sided")#shows no significant difference 
###
###Question 2
edata$Per_capita_Coal = edata$TotalCoal / edata$Population
coast2 = subset(edata, Coast == "1")
no_coast2 = subset(edata, Coast == "0")
coast_coal = coast1[, "Per_capita_Coal"]
no_coast_coal = no_coast1[, 9]
qqnorm(edata[, 9])#testing for normaility
qqline(edata[, 9], col="red")
shapiro.test(edata[, 9])
var.test(coast_coal, no_coast_coal)#testing for equal variances
t.test(coast_coal, no_coast_coal)#t-test revealed significant difference between coal consumption and state on/off coast.
boxplot(coast_coal, no_coast_coal, names = c("Coast", "No Coast"), ylab = "Per Capita Coal Consumption")
###
###Question 3
aov_coal = aov(Per_capita_Coal ~ Region, data = edata)#running anova
summary(aov_coal)#non significant p-value
TukeyHSD(aov_coal)#just curious
shapiro.test(edata$Per_capita_Coal)#testing for normality
East1 = subset(edata, Region == "East")
West1 = subset(edata, Region == "West")
Midwest1 = subset(edata, Region == "Midwest")
South1 = subset(edata, Region == "South")
East = East1[, "Per_capita_Coal"]
West = West1[, "Per_capita_Coal"]
Midwest = Midwest1[, "Per_capita_Coal"]
South = South1[, "Per_capita_Coal"]
qqnorm(East)#testing for normality
qqline(East, col="red")
shapiro.test(East)
qqnorm(West)
qqline(West, col="red")
shapiro.test(West)
qqnorm(Midwest)
qqline(Midwest, col="red")
shapiro.test(Midwest)
qqnorm(South)
qqline(South, col="red")
shapiro.test(South)
boxplot(East, West, Midwest, South, names = c("East", "West", "Midwest", "South"), ylab = "Per Capita Coal Consumption")
var.test(East, West)#pairwise variance tests.
var.test(East, Midwest)
var.test(East, South)
var.test(West, Midwest)
var.test(West, South)
var.test(Midwest, South)
###
###Question 4
edata$Per_capita_GDP = edata$TotalGDP / edata$Population
cor1 = cor(edata$Per_capita_Coal, edata$Per_capita_GDP)
cor1#0.03598-weak positive correlation.
###
# Housing data for question 5-9
# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
###
###Question 5
mod = lm(medv ~ ptratio + crim + lstat, data = hdata)
cor(hdata[, c("ptratio", "crim", "lstat")], use="na.or.complete")#no possibly high correlations
#install.packages("fmsb")
library(fmsb)
VIF(lm(medv ~ ptratio + crim + lstat, data = hdata))#2.334327. Not high enough to worry about multicollinearity. 
###
###Question 6# Ploting individual variables on dependent variable.
plot(medv~ptratio, data = hdata, xlab = "Pupil to Student Ratio", ylab = "Medium Housing Prices", main = "Pupil to Student Ratio Effect on Medium Housing Prices"))
abline(lm(medv~ptratio, data = hdata))
plot(medv~crim, data = hdata, xlab = "Crime Rate", ylab = "Medium Housing Prices", main = "Crime Rate Effect on Medium Housing Prices")
abline(lm(medv~crim, data = hdata))
plot(medv~lstat, data = hdata, xlab = "% Lower Status of Population", ylab = "Medium Housing Prices", main = "% Lower Status of Population Effect on Medium Housing Prices")
abline(lm(medv~lstat, data = hdata))
summary(lm(medv~ptratio, data =hdata))
summary(lm(medv~crim, data =hdata))
summary(lm(medv~lstat, data =hdata))
###
###Question 7
mod = lm(medv ~ ptratio + crim + lstat, data = hdata)
summary(mod)
install.packages("lmtest")
library(lmtest)
bptest(mod)#Checking linear model assumptions. All tests show assumption violations. 
dwtest(mod)
shapiro.test(residuals(mod))
###
###Question 8
mod = lm(medv ~ ptratio + crim + lstat, data = hdata)#multiple regression model.
summary(mod)
mod1 = lm(medv ~ ptratio + crim * lstat, data = hdata)#multiple regression model with interaction term.
summary(mod1)