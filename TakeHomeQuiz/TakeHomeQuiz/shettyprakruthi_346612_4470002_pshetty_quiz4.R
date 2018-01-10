# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
library(reshape2)
library(plyr)
library(magrittr)
library(car)
library(lmtest)
#-------------------------- Question 1------------------------#
#NULL : per capita energy consumption for a state on coast is not different than that of a state not on coast
#A : per capita energy consumption for a state on coast is different than that of a state not on coast

edata = edata %>%
  mutate(energy_pc = TotalEnergy/Population)
head(edata)

edata.coast = subset(edata, Coast==1)
edata.notcoast = subset(edata, Coast==0)

hist(edata.coast[,"energy_pc"], breaks=15, col="light blue", xlim=c(0, 1), ylim=c(0,8), xlab="energy per capita", main="")
par(new=TRUE)
hist(edata.notcoast[,"energy_pc"], breaks=15, col="pink", xlim=c(0, 1), ylim=c(0,8), xlab="", main="")
abline(v=mean(edata.notcoast[,"energy_pc"]), col="red")
abline(v=mean(edata.coast[,"energy_pc"]), col="blue")
legend("topright", legend = c("Mean for coast", "Mean for not on coast"),text.col=c("blue", "red"))

#Checking for assumptions

var.test(edata.coast$energy_pc,edata.notcoast$energy_pc)
#p-value = 0.5098, Since p-value is above 0.05 we accept the null hypothesis thus both the samples: edata.notcoast & edata.coast have equal variance

shapiro.test(edata.coast$energy_pc)
#p-value: 5.037e-06
shapiro.test(edata.notcoast$energy_pc)
#p-value: 3.627e-05
#Since the p-value is less than 0.05 both the samples are not normal. Also sample size is less than 30!
qqnorm(edata.coast$energy_pc)
qqline(edata.coast$energy_pc, col="red")
qqnorm(edata.notcoast$energy_pc)
qqline(edata.notcoast$energy_pc, col="red")
#The above plots show that the small samples are normal.

#Assumed that each observation is sampled independently

#2-Sample T-test (Not paired)
t.test(edata.coast$energy_pc,edata.notcoast$energy_pc, paired=FALSE)
#p-value: 0.2245. Since p-value is greater than 0.05 we accept the null hypothesis that per capita energy consumption for a state on coast is not different than that of a state not on coast.

#--------------------------End Question 1------------------------#


#--------------------------Question 2------------------------#
#NULL : per capita coal consumption for a state on coast is not different than that of a state not on coast
#A : per capita coal consumption for a state on coast is different than that of a state not on coast
edata = edata %>%
  mutate(coal_pc = TotalCoal/Population)
head(edata)

edatac.coast = subset(edata, Coast==1)
edatac.notcoast = subset(edata, Coast==0)

hist(edatac.coast[,"coal_pc"], breaks=15, col="light blue", xlim=c(0, 1), ylim=c(0,8), xlab="coal per capita", main="")
par(new=TRUE)
hist(edatac.notcoast[,"coal_pc"], breaks=15, col="pink", xlim=c(0, 1), ylim=c(0,8), xlab="", main="")
abline(v=mean(edatac.notcoast[,"coal_pc"]), col="red")
abline(v=mean(edatac.coast[,"coal_pc"]), col="blue")
legend("topright", legend = c("Mean for coast", "Mean for not on coast"),text.col=c("blue", "red"))

#Checking for assumptions

var.test(edatac.coast$coal_pc,edatac.notcoast$coal_pc)
#p-value = 5.995e-13, Since p-value is less than 0.05 we reject the null hypothesis thus both the samples: edata.notcoast & edata.coast dont have equal variance

shapiro.test(edatac.coast$coal_pc)
#p-value: 0.001023
shapiro.test(edatac.notcoast$coal_pc)
#p-value: 6.211e-07
#Since the p-value is less than 0.05 both the samples are not normal. Also sample size is less than 30!
qqnorm(edatac.coast$coal_pc)
qqline(edatac.coast$coal_pc, col="red")
qqnorm(edatac.notcoast$coal_pc)
qqline(edatac.notcoast$coal_pc, col="red")
#The above plots show that the small samples are normal.


#Assumed that each observation is sampled independently

#2-Sample T-test (Not paired)
t.test(edatac.coast$coal_pc,edatac.notcoast$coal_pc, paired=FALSE, var.equal = FALSE)
#p-value: 0.001936 Since p-value is lesser than 0.05 we reject the null hypothesis that per capita coal consumption for a state on coast is not different than that of a state not on coast.


#--------------------------End Question 2------------------------#


#-------------------------- Question 3------------------------#
#Checking for assumptions
leveneTest(coal_pc~Region, data=edata)
#p:0.5202, since p-value is greater than 0.05, null hypothesis is true and the variance of coal_pc variable for different regions is not different.

shapiro.test(edata$coal_pc)
#p-value:6.024e-11
#Since the p-value is less than 0.05 data are not normallly distributed.
qqnorm(edata$coal_pc)
qqline(edata$coal_pc, col="red")
#Although the above plot shows that they are almost normal.


#Assumed that each observation is sampled independently

boxplot(coal_pc~Region, data=edata, xlab="Region", ylab="coal per capita")
modQ1_3=aov(coal_pc~Region, data=edata)
summary(modQ1_3)
#Since the p-value is greater than 0.05, we accept the null hypothesis, coal consumption per capita doesnt differ among different regions.

#Since the data is not normal, using the kruskal.test
kruskal.test(coal_pc~Region, data=edata)
#p:p-value = 0.00042, null hypothesis is false, per capita coal consumption differs depending on the region in which a state is found
#--------------------------End Question 3------------------------#

#--------------------------Question 4------------------------#
edata = edata %>%
  mutate(gdp_pc = TotalGDP/Population)
head(edata)

cor(edata$coal_pc,edata$gdp_pc)
#0.03598182
#As the correlation coefficient is very small, i.e. 0.03598, per capita coal use and per capita GDP are not strongly correlated.
#--------------------------End Question 4------------------------#



# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)



#--------------------------Question 5------------------------#
cor(hdata$zn, hdata$age)
#-0.5556789, the coefficient value is high thus these 2 variables are negatively correlated.
cor(hdata$zn, hdata$b)
#0.1503797, the coefficient value is very low thus these 2 variable are not that correlated.
cor(hdata$age, hdata$b)
#-0.2237652, the coefficient value is very low thus these 2 variable are not that correlated.

vif = 1/(1 - summary(lm(medv~zn+age+b, dat=hdata))$r.squared)
vif
#1.201813, since VIF value is less than 5 the collinearity of the variable does not affect the model.

#--------------------------End Question 5------------------------#


#--------------------------Question 6------------------------#
plot(medv~zn, dat=hdata)
abline(lm(medv~zn, dat=hdata), col="red")

plot(medv~age, dat=hdata)
abline(lm(medv~age, dat=hdata), col="red")

plot(medv~b, dat=hdata)
abline(lm(medv~b, dat=hdata), col="red")

#--------------------------End Question 6------------------------#


#--------------------------Question 7/8/9------------------------#

mod = lm(medv~zn+age+b, dat=hdata)

dwtest(mod,alternative=c("two.sided"))
#p-value < 2.2e-16, since p-value is less than 0.05, the alternative hypothesis is true and there exists autocorrelation and the residuals are not indeppendent.

shapiro.test(residuals(mod))
#p:2.2e-16, The residuals are not normally distributed.

bptest(mod)
#p:0.001001, the residuals are not homostedastic.

plot(mod)

summary(mod)

#--------------------------End Question 7/8/9------------------------#



