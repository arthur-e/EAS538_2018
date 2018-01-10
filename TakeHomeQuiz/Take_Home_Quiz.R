library(dplyr)
library(fmsb)

### READ IN DATA + ADD IN NECESSARY PER CAPITA COLUMNS ####
edata=read.csv('/Users/mehajain/Desktop/energy_data.csv')
edata=mutate(edata,TotalEnergypc=TotalEnergy/Population,TotalGDPpc=TotalGDP/Population,TotalCoalpc=TotalCoal/Population)

### if the per capita calculation is wrong, take 1 pt once and 2 pt at most. 

####################################################
################ T TEST SECTION ####################
####################################################

##### Q1. Does per capita energy consumption vary based on whether the state is on a coast or not?
# a. write null and alternate hypothesis
# b. create a visual
boxplot(TotalEnergypc~Coast,data=edata)
# c. check assumptions
# check for equal variances - yes equal
shapiro.test()
var.test(TotalEnergypc~Coast,data=edata)
# d. run t-test with equal variances - no difference
### reason must be stated. 
t.test(TotalEnergypc~Coast,var.equal=TRUE,data=edata)

##### Q2. Does per capita coal consumption vary based on whether the state is on a coast or not?
# a. write null and alternate hypothesis
# b. create a visual
boxplot(TotalCoalpc~Coast,data=edata)
# c. check assumptions
# check for equal variances - not equal
var.test(TotalCoalpc~Coast,data=edata)
# d. run t-test with equal variances - yes difference. higher coal consumption not on the coast
t.test(TotalCoalpc~Coast,var.equal=FALSE,data=edata)

##################################################
################## ANOVA SECTION #################
##################################################

#### Q3. Does the total per capita Coal Consumption vary based on Region?
# a. write null and alternate hypothesis
# b. make visual
boxplot(TotalGDPpc~Region,data=edata)
# c. check assumptions
# equal variance - yes
leveneTest(TotalGDPpc~Region,data=edata)
# d. run anova
anova(aov(TotalGDPpc~Region,data=edata))

###### Q. Does GDP per capita vary by region?
# a. write null and alternate hypothesis
# b. make visual
boxplot(TotalCoalpc~Region,data=edata)
# c. check assumptions
# equal variance - yes
leveneTest(TotalCoalpc~Region,data=edata)
# d. run anova
anova(aov(TotalCoalpc~Region,data=edata))
###### Q. Does GDP per capita vary by region?

#### Q4. What is the correlation between energy use per capita and GDP? Does it seem like a strong correlation?
cor(edata$TotalGDPpc,edata$TotalCoalpc)

#### HOUSING DATA
house=read.csv('/Users/mehajain/Desktop/housingdata.csv')
house=na.omit(hdata)

#### Q5 look at correlations + pick non-correlated model
cor(house,house)
model=lm(medv~nox+rm+crim,data=house)
VIF(model) # not too correlated 

#### Q6 look at visual plots of the relationships and state what they are
par(mfrow=c(2,2))
plot(medv~nox,data=house)
plot(medv~rm,data=house)
plot(medv~crim,data=house)

#### Q7 check the assumptions of your model
plot(model) # looks like the values are not entirely normal and there may be some problems with heteroscedasticity

#### Q9 Interpret model results
summary(model)

#### Q10
summary(model)
