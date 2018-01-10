library(RCurl)
library(dplyr)
# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

#Question 1
edata = mutate(edata, EnergyPerCapita=TotalEnergy/Population)
notcoast = subset(edata, Coast==0)
coast = subset(edata, Coast==1)
#b) Please create a visual plot to answer this questioncoast = subset(edata, Coast==1)
#make boxplot comparing the two
boxplot(EnergyPerCapita~Coast, data=edata, xlab="Coastal Status", ylab="Per Capita Energy Consumption", main="Energy Data")
#c) Please decide what statistical test to use and check whether your data meet the assumptions to run this test 
#Test for normality:
shapiro.test(coast$EnergyPerCapita)
shapiro.test(notcoast$EnergyPerCapita)
#The data are not normal: in fact they have a positive skew.
#We will transform the data.
notcoast_log = mutate(notcoast, LogEnergyPC=log(EnergyPerCapita))
coast_log = mutate(coast, LogEnergyPC=log(EnergyPerCapita))
#This is not perfect but it is better than before:
shapiro.test(notcoast_log$LogEnergyPC)
shapiro.test(coast_log$LogEnergyPC)
#Test for equal variance:
var.test(coast$EnergyPerCapita, notcoast$EnergyPerCapita)
#We have insufficient evidence to reject the null hypothesis that the variances are equal.
#So we will proceed as though they have equal variance.
#The third assumption is that each observation is observed independently. This is difficult to test, so we will proceed as though they were.
#d. Please run the test and interpret the result.
t.test(coast_log$LogEnergyPC, notcoast_log$LogEnergyPC, paired=FALSE)
#This means that we do not have enough evidence to reject the null hypothesis that the mean energy consumption of coastal and non-coastal states are different from each other.

#Question 2
edata = mutate(edata, CoalPerCapita=TotalCoal/Population)
notcoast = subset(edata, Coast==0)
coast = subset(edata, Coast==1)
#b. Please construct a visual plot to answer this question.
boxplot(CoalPerCapita~Coast, data=edata, xlab="Coastal Status", ylab="Per Capita Coal Consumption", main="Coal Data")
#c) Please decide what statistical test to use and check whether your data meet the assumptions to run this test 
#You should use a t-test because the independent variable is categorical (with only two values) and the dependent variable is continuous.
#Test for normality:
shapiro.test(coast$CoalPerCapita)
shapiro.test(notcoast$CoalPerCapita)
#The data are not normal. We will transform them.
notcoast_cubert = mutate(notcoast, CubertCoalPC=CoalPerCapita^(1/3))
coast_cubert = mutate(coast, CubertCoalPC=CoalPerCapita^(1/3))
#Test them again:
shapiro.test(coast_cubert$CubertCoalPC)
shapiro.test(notcoast_cubert$CubertCoalPC)
#That's better.
#d. Run the statistical test and interpret your result.
t.test(coast_cubert$CubertCoalPC, notcoast_cubert$CubertCoalPC, paired=FALSE)
#The p-value is < .05, meaning that we have enough evidence to reject the null hypothesis, and accept the alternate hypothesis that the mean coal consumption is different in coastal states than non-coastal states.

#Question 3.

#b. Please create a visual to answer this question.
boxplot(data=edata, CoalPerCapita~Region, xlab="Region", ylab="Per Capita Coal Consumption", main="Coal By Region")
#c. Decide what statistical tests to use and check whether your data meet the assumptions to run this test.
#You should use an ANOVA because the independent variable is categorical (with more than two values) and the dependent variable is continuous.
#Let's test normality:
shapiro.test(edata$CoalPerCapita)
#The data are not normal. Even though this is not a big deal for anova data, let's transform them using a cube root function (since the square root is not enough to return them to normality).
edata_cubert = mutate(edata,CubeRtCoalPC=(CoalPerCapita^(1/3)))
shapiro.test(edata_cubert$CubeRtCoalPC)
#That's better.
#We will also check for equal variances:
leveneTest(CubeRtCoalPC~Region, data=edata_cubert)
#The p-value is greater than .05, so we can't safely reject the null hypothesis that the variances are equal.
#d. Run the statistical test.
coal_aov = aov(CubeRtCoalPC~Coast, data=edata_cubert)
summary(coal_aov)
#The p value is less than .05, so there is a significant difference between mean coal consumption across different state's regions.

#Question 4
#4.	What is the correlation between per capita coal use and per capita GDP? Does this seem like a strong correlation to you? 
edata = mutate(edata,GDPPerCapita=TotalGDP/Population)
cor(edata$GDPPerCapita, edata$CoalPerCapita)
#This doesn't seem like a strong correlation.

#Question 5
#Please select three covariates that you will include in your model as independent variables. 
##Let's use AGE, NOX, and CRIM.
#Please check if these variables are highly correlated with one another to make sure you do not run into problems of multi-collinearity.
cor(hdata[,c("age","nox","crim","medv")])
##None of them have a correlation stronger than -.33, so I feel confident that we will not have to deal with multi-collinearity.
#Check if this model has issues with multi-collinearity using the variance inflation factor.
vif.age = 1/(1 - summary(lm(medv~age, dat=hdata))$r.squared)
vif.age
vif.nox = 1/(1 - summary(lm(medv~nox, dat=hdata))$r.squared)
vif.nox
vif.crim = 1/(1 - summary(lm(medv~crim, dat=hdata))$r.squared)
vif.crim

#Question 6
#6.	Plot the relationship between each of your three independent variables and the dependent variable (medv).
plot(medv~age, data=hdata)
plot(medv~nox, data=hdata)
plot(medv~crim, data=hdata)

#Question 7
#I have checked the following assumptions
#Multicollinearity-- see question 5. The VIF factors were all small, so there is very little collinearity.
#Homoscedasticity: we will use var.test for this

#Normality: we will use shapiro.test for this.
mod = lm(medv~age+nox+crim, data=hdata)
shapiro.test(residuals(mod))
#The residuals are not normal. Let's transform the data.
mod_log = lm(log(medv)~age+nox+crim, data=hdata)
shapiro.test(residuals(mod_log))
#They're still not normal. We will proceed as is for now, since we have a large sample size, but we must be careful when interpreting results.
summary(mod)

#8. Interpret the results.
#With no other input from other variables, the median value of housing prices would be $33.78 thousand dollars.
#Every one-unit increase in proportion of old homes causes the housing price to decrease by $-.041 thousand dollars.
#Every one-unit increase in nitric oxide concentration causes the housing price to decrease by $12.46 thousand dollars.
#Every one-unit increase in per capita crime rate causes housing prices to decrease by $-.44 thousand dollars.
#All of these effects are significant.

#Add an interaction term:
mod_interact = lm(medv~nox*chas, data=hdata)
summary(mod_interact)
#Houses located next to the Charles River with 0 nitric oxide concentrations have a median value of $38.17 thousand dollars.
#Each additional unit of nitric oxide concentration decreases median property values by $27.62 thousand dollars.
#Houses located next to the Charles River are worth $7.78 thousand dollars more than houses that aren't located next to the river.