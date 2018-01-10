### Questions 1-4 ###
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(edata)

install.packages("reshape2")
install.packages("plyr")
install.packages("magrittr")
library(reshape2)
library(plyr)
library(magrittr)


### Question 1 ###

## Calculating per capita energy consumption
edata = edata %>%
  mutate(PerCapita.E = TotalEnergy/Population)
head(edata)

## Subsetting data
e.coast = subset(edata, Coast=="1")
head(e.coast)
e.nocoast = subset(edata, Coast=="0")
head(e.nocoast)

## Visualizing data
hist(e.coast[,"PerCapita.E"], breaks=15, col="light blue", xlim=c(0, 1), ylim=c(0,9), xlab="Per Capita Energy Consumption", main="Distribution of States' Per Capita Energy Consumption")
abline(v=mean(e.coast[,"PerCapita.E"]), col="blue")
par(new=TRUE)
hist(e.nocoast[,"PerCapita.E"], breaks=15, xlim=c(0, 1), ylim=c(0,9), xlab="", main="")
abline(v=mean(e.nocoast[,"PerCapita.E"]), col="dark green")
legend("topright", legend = c("Coastal Mean", "Non-Coastal Mean"),text.col=c("blue", "black"))
# It appears that the data are skewed to the right
# The data may not be normally distributed

## Checking assumptions
shapiro.test(e.coast[,"PerCapita.E"])
# Reject null hypothesis that data are normally distributed
# p-value = 5.037e-06
# Coastal data are not normally distributed

shapiro.test(e.nocoast[,"PerCapita.E"])
# Reject null hypothesis that data are normally distributed
# p-value = 3.627e-05
# Non-coastal data are not normally distributed

var.test(e.coast[,"PerCapita.E"], e.nocoast[,"PerCapita.E"])
# Fail to reject null hypothesis that variances are equal
# p-value = 0.5098
# Variances are equal

## Must use a Mann-Whitney-Wilcoxon Test
wilcox.test(PerCapita.E ~ Coast, data=edata)
# Reject null hypothesis that per capita energy consumption is the same
# Per capita energy consumption in coastal states is significantly different (p-value = 0.008417) from per capita energy consumption in non-coastal states.

### Question 2 ###

## Calculating per capita coal consumption
edata = edata %>%
  mutate(PerCapita.C = TotalCoal/Population)
head(edata)

## Subsetting data
e.coast.2 = subset(edata, Coast=="1")
head(e.coast.2)
e.nocoast.2 = subset(edata, Coast=="0")
head(e.nocoast.2)

## Visualizing data
hist(e.coast.2[,"PerCapita.C"], breaks=15, col="light blue", xlim=c(0, 1), ylim=c(0,8), xlab="Per Capita Coal Consumption", main="Distribution of States' Per Capita Coal Consumption")
abline(v=mean(e.coast.2[,"PerCapita.C"]), col="blue")
par(new=TRUE)
hist(e.nocoast.2[,"PerCapita.C"], breaks=40, xlim=c(0, 1), ylim=c(0,8), xlab="", main="")
abline(v=mean(e.nocoast.2[,"PerCapita.C"]), col="dark green")
legend("topright", legend = c("Coastal Mean", "Non-Coastal Mean"),text.col=c("blue", "black"))
# It appears that the data are skewed to the right
# The data may not be normally distributed

## Checking assumptions
shapiro.test(e.coast.2[,"PerCapita.C"])
# Reject null hypothesis that data are normally distributed
# p-value = 0.001023
# Coastal data are not normally distributed

shapiro.test(e.nocoast.2[,"PerCapita.C"])
# Reject null hypothesis that data are normally distributed
# p-value = 6.211e-07
# Non-Coastal data are not normally distributed

var.test(e.coast.2[,"PerCapita.C"], e.nocoast.2[,"PerCapita.C"])
# Reject null hypothesis that variances are equal
# p-value = 5.995e-13
# Variances are not equal

## Using a two-sample T-test despite the fact that two assumptions were violated
t.test(e.coast.2[,"PerCapita.C"], e.nocoast.2[,"PerCapita.C"], paired=FALSE)
# Reject null hypothesis that per capita energy consumption is the same
# Per capita coal consumption in coastal states is significantly different (p-value = 0.001936) from per capita energy consumption in non-coastal states.

### Question 3 ###

## Subsetting data
East = subset(edata, Region=="East")
Midwest = subset(edata, Region=="Midwest")
South = subset(edata, Region=="South")
West = subset(edata, Region=="West")

## Visualizing data
boxplot(PerCapita.C~Region, data=edata, main="States' Per Capita Coal Consumption by Region", xlab="Region", ylab="Per Capita Coal Consumption")

## Checking assumptions
shapiro.test(East[,"PerCapita.C"])
# Reject null hypothesis that data are normally distributed
# p-value = 5.535e-06
# East data are not normally distributed

shapiro.test(Midwest[,"PerCapita.C"])
# Reject null hypothesis that data are normally distributed
# p-value = 0.0001758
# Midwest data are not normally distributed

shapiro.test(South[,"PerCapita.C"])
# Reject null hypothesis that data are normally distributed
# p-value = 5.689e-05
# South data are not normally distributed

shapiro.test(West[,"PerCapita.C"])
# Reject null hypothesis that data are normally distributed
# p-value = 1.103e-05
# West data are not normally distributed

install.packages("car")
library(car)
leveneTest(PerCapita.C~Region, data=edata)
# Fail to reject null hypothesis that the variances are equal
# p-value = 0.5202
# Variances are equal

## Using an ANOVA despite the fact that the data are not normally distributed
aov.1 = aov(PerCapita.C~Region, data=edata)
summary(aov.1)
# Fail to reject null hypothesis that mean per capita coal consumption is equal among the various regions
# Per capita coal consumption is not significantly different (p-value = 0.262) among the various regions

### Question 4 ###

## Calculating per capita GDP
edata = edata %>%
  mutate(PerCapita.GDP = TotalGDP/Population)
head(edata)

## Visualizing data
plot(PerCapita.GDP~PerCapita.C, data=edata)
abline(lm(PerCapita.GDP~PerCapita.C, data=edata), col="red")
# Based on the plot of per capita GDP and per capita coal consumption, the two variables do not see to have a strong linear correlation.

# Calculating Pearson correlation coefficient
cor(edata[, c("PerCapita.GDP", "PerCapita.C")], use="na.or.complete")
# Correlation coefficient = 0.03598182
# Because the value is close to 0, these two variables are weakly correlated.


### Questions 5-9 ###

hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(hdata)


### Question 5 ###

## Checking for highly-correlated independent variables
cor(hdata[, c("crim", "rm", "dis")], use="na.or.complete")
# It appears that crim and dis might be too highly correlated (0.4619674). This might pose issues with multi-collinearity.

## Calculating VIF values
mod = lm(medv~crim+rm+dis, dat=hdata)
vif.rad = 1/(1-summary(mod)$r.squared)
vif.rad
# VIF = 2.403192
# Because the VIF value is less than 3, it is ok to include all three independent variables in the linear model


### Question 6 ###

## Relationship between tax and medv
plot(medv~crim, data=hdata, main="Relationship between per capita crime rate by town and housing value")
# The plot indicates a weak negative relationship between per capita crime rate by town and home value.

## Relationship between rm and medv
plot(medv~rm, data=hdata, main="Relationship between average number of rooms per dwelling and housing value")
# The plot indicates a strong positive relationship between the average number of rooms per dwelling increases and home value.

## Relationship between rad and medv
plot(medv~dis, data=hdata, main="Relationship between weighted distances to five Boston employment centers and housing value")
# The plot does not indicate a strong relationship between weighted distance to five Boston employment centers and housing value. 


### Question 7 ###

## Visualizing the residuals
plot(residuals(mod))
# It appears that there are several outliers that may violate the assumptions of a linear regression model.

install.packages("lmtest")
library(lmtest)

# Checking independence of residuals
dwtest(mod, alternative=c("two.sided"))
# Reject null hypothesis that errors are independent
# p-value = 2.2e-16
# Errors are not independent

# Checking for homoscedasticity
install.packages("lmtest")
library(lmtest)

bptest(mod)
# Reject null hypothesis that errors are homoscedastic
# p-value = 4.568e-07
# Errors are heteroscedastic

# Checking for normality
shapiro.test(residuals(mod))
# Reject null hypothesis that errors are homoscedastic
# p-value = 4.568e-07
# Errors are heteroscedastic


### Question 8 ###
mod = lm(medv~crim+rm+dis, dat=hdata)
summary(mod)
# When the crime rate, rooms per household, and distance to employment centers are all zero, home value is -34.3862 units.
# The intercept is significantly (p-value = 2e-16) different from 0. 
# The relationship between crime rate and home value is significant (p-value = 2.15e-09), such that when the crime rate increases by 1 unit, home value decreases by 0.7441 units. 
# The relationship between rooms per household and home value is significant (p-value = 2e-16), whereby as the number of rooms increases by one unit, home value increases by 9.4879 units. 
# Distance to employment centers and home value are not significantly (p-value = 0.0917) related. 
# The adjusted R-squared value indicates that the model explains 58.11% of the variation in home value. 
# The null model (i.e. the model with the intercept only) can be rejected (p-value = 2e-16). Thus, including the other independent variables in the full model explains significantly more of the variation in home value than the null model alone.


