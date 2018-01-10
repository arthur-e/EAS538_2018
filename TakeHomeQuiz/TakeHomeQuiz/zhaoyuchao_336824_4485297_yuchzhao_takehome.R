###Question 1 
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

### creat a new colume in the dataset for per captia enegrgy consumption in each state 
perEnergy= transform(edata, percaptiaEnergy = TotalEnergy / Population)
head(perEnergy)

###subset data from coastal and noncoastal area
perenergy.coast = subset(perEnergy, Coast==1)
perenergy.noncoast = subset(perEnergy, Coast==0)

###plot the the two ditributions of "percaptiaEnergy"
hist(perenergy.coast[,"percaptiaEnergy"], col="light blue",ylim=c(0,12), xlab="per captia energy consumption", main="")
abline(v=mean(perenergy.coast[,"percaptiaEnergy"]), col="blue")
par(new=TRUE)
hist(perenergy.noncoast[,"percaptiaEnergy"],border="light green",ylim=c(0,12),xlab="", main="")
abline(v=mean(perenergy.noncoast[,"percaptiaEnergy"]), col="dark green")
legend("topright", legend = c("coast mean", "noncoast mean"),text.col=c("blue", "dark green"), cex=0.7)

###normality test
shapiro.test(perenergy.coast[,"percaptiaEnergy"])
###p-value = 5.037e-06
shapiro.test(perenergy.noncoast[,"percaptiaEnergy"])
###p-value = 3.627e-05
qqnorm(perenergy.coast[,"percaptiaEnergy"])
qqline(perenergy.coast[,"percaptiaEnergy"])
qqnorm(perenergy.noncoast[,"percaptiaEnergy"])
qqline(perenergy.noncoast[,"percaptiaEnergy"])
###the value did not past the shapiro test, but accodring to qqplot most of the value are close to the normal line
###there are serval outlier that influcen the normality, but could be accepted.

###vriation test
var.test(perenergy.coast[,"percaptiaEnergy"], perenergy.noncoast[,"percaptiaEnergy"])
###p-value = 0.5098, two group samples have same vriacnce 

t.test(perenergy.coast[,"percaptiaEnergy"], perenergy.noncoast[,"percaptiaEnergy"],var.equal=TRUE)
###p-value = 0.2179

###Question 2

### creat a new colume in the dataset for per captia coal consumption in each state 
coalEnergy= transform(edata, percoalconsumption = TotalCoal / Population)
head(coalEnergy)

###subset data from coastal and noncoastal area
percoal.coast = subset(coalEnergy, Coast==1)
percoal.noncoast = subset(coalEnergy, Coast==0)

###plot the the two ditributions of "percoalconsumption"
hist(percoal.coast[,"percoalconsumption"], col="light blue",xlim=c(0,1), ylim=c(0,16), xlab="per captia coal consumption", main="")
abline(v=mean(percoal.coast[,"percoalconsumption"]), col="blue")
par(new=TRUE)
hist(percoal.noncoast[,"percoalconsumption"],xlim=c(0,1),ylim=c(0,16),xlab="", main="",border="light green")
abline(v=mean(percoal.noncoast[,"percoalconsumption"]), col="dark green")
legend("topright", legend = c("coast mean", "noncoast mean"),text.col=c("blue", "dark green"),cex=1)

###normality test
shapiro.test(percoal.coast[,"percoalconsumption"])
###p-value = 0.001023
shapiro.test(percoal.noncoast[,"percoalconsumption"])
###p-value = 6.211e-07
qqnorm(percoal.coast[,"percoalconsumption"])
qqline(percoal.coast[,"percoalconsumption"])
qqnorm(percoal.noncoast[,"percoalconsumption"])
qqline(percoal.noncoast[,"percoalconsumption"])
###there are some outliers but the major part is on the line, considering the samples size is not large enough, this can be accept.

###vriation test
var.test(percoal.coast[,"percoalconsumption"], percoal.noncoast[,"percoalconsumption"])
###p-value = 5.995e-13, two group samples have different vriacnce 

t.test(percoal.coast[,"percoalconsumption"], percoal.noncoast[,"percoalconsumption"])
###p-value = 0.001936

###Question3
###subset by different region 
coalEnergy1 = subset(coalEnergy, Region=="East")
coalEnergy2 = subset(coalEnergy, Region=="Midwest")
coalEnergy3 = subset(coalEnergy, Region=="South")
coalEnergy4 = subset(coalEnergy, Region=="West")
###visual plot
boxplot(percoalconsumption~Region, data=coalEnergy, xlab="Region", ylab="per captia coal consumption")
###normality test indicate that majrity part fit the line
shapiro.test(coalEnergy1[,"percoalconsumption"]) 
### p-value = 5.535e-06
shapiro.test(coalEnergy2[,"percoalconsumption"])
###p-value = 0.0001758
shapiro.test(coalEnergy3[,"percoalconsumption"])
### p-value = 5.689e-05
shapiro.test(coalEnergy4[,"percoalconsumption"])
### p-value = 1.103e-05
qqnorm(coalEnergy1[,"percoalconsumption"])
qqline(coalEnergy1[,"percoalconsumption"])
qqnorm(coalEnergy2[,"percoalconsumption"])
qqline(coalEnergy2[,"percoalconsumption"])
qqnorm(coalEnergy3[,"percoalconsumption"])
qqline(coalEnergy3[,"percoalconsumption"])
qqnorm(coalEnergy4[,"percoalconsumption"])
qqline(coalEnergy4[,"percoalconsumption"])
###vriation test
install.packages("Rcmdr")
library(Rcmdr)
levene.test(percoalconsumption~Region,data=coalEnergy)
###p-value =0.5202, the variance is same

###anova test
kruskal.test(percoalconsumption~Region, data=coalEnergy)

###Question 4
### creat 2 new colume in the dataset contain per captia coal consumption and per cpatia GDP in each state 
CoalGDP= transform(edata, percoalconsumption = TotalCoal / Population,perGDP=TotalGDP/Population)
head(CoalGDP)
cor(CoalGDP$percoalconsumption,CoalGDP$perGDP)
### correlation coefficient=0.03598182
plot(perGDP~percoalconsumption, data=CoalGDP)
abline(lm(perGDP~percoalconsumption, data=CoalGDP), col="red")


###Question5
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(hdata)

###correlation value
cor(hdata[,c("rm","nox","crim")], use="na.or.complete")
###         rm        nox       crim
###rm    1.0000000 -0.2645944 -0.1424577
###nox  -0.2645944  1.0000000  0.6369411
###crim -0.1424577  0.6369411  1.0000000

###VIF value
vif.mod = 1/(1 - summary(lm(medv~rm+nox+crim, dat=hdata))$r.squared)
vif.mod
###2.395009 , the result<5, which means we do not need worry about the corlinear among variables

###Question6
plot(medv~rm, data=hdata)
abline(lm(medv~rm, data=hdata), col="red")

plot(medv~nox, data=hdata)
abline(lm(medv~nox, data=hdata), col="red")

plot(medv~crim, data=hdata)
abline(lm(medv~crim, data=hdata), col="red")

mod = lm(medv~rm+nox+crim, dat=hdata)
summary(mod)

###Question7
plot(residuals(mod))
###Durbin-Watson statistic to detect the existence of autocorrelation
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
### p-value < 2.2e-16,the residuals show a clear temporal autocorrelation

### test for homoscedasticity 
bptest(mod)
###p-value = 1.969e-06, the results show that the residuals are not homoscedastic. 

###test for normality
shapiro.test(residuals(mod))
###p-value < 2.2e-16, residuals are not normally distributed.

###Try to transform---Fail
###logtransformation 
mod1 = lm(log(medv)~nox+dis+b, dat=hdata)
shapiro.test(residuals(mod1))
###Square root transformation
mod2 = lm(sqrt(medv)~rm++crim, dat=hdata)
shapiro.test(residuals(mod2))
###boxcox
install.packages("car")
library(car)
mod.trans=update(mod,.~.+boxCoxVariable(medv))
shapiro.test(residuals(mod.trans))

###Question 8
mod.bonus = lm(medv~rm+nox+crim+nox*crim, dat=hdata)
summary(mod.bonus)

###Question 9
mod.a = lm(medv~rm, data=hdata)
mod.b = lm(medv~rm+nox, data=hdata)
anova(mod.a,mod.b,mod)









