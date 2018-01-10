# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(edata)

#create per capita variables
library(dplyr)

edata = edata %>%
  mutate(pcenergy = TotalEnergy/Population)

edata = edata %>%
  mutate(pccoal = TotalCoal/Population)

edata = edata %>%
  mutate(pcGDP = TotalGDP/Population)

head(edata)

coast = subset(edata, Coast=="1")
ncoast = subset(edata, Coast=="0")

#Question 1

hist(coast[,"pcenergy"], breaks=15, col="light blue", xlim=c(0, 1), ylim=c(0,10), xlab="per capita energy consumption", main="")
abline(v=mean(coast[,"pcenergy"]), col="blue")
par(new=TRUE)
hist(ncoast[,"pcenergy"], breaks=15, col="light green", xlim=c(0, 1), ylim=c(0,10), xlab="", main="")
abline(v=mean(ncoast[,"pcenergy"]), col="dark green")
legend("topright", legend = c("Coastal state mean", "Non-Coastal state mean"),text.col=c("blue", "dark green"))

shapiro.test(edata[,"pcenergy"]) #p-value = 8.808e-08 (greater than .05), so the distribution is not normal but we have large enough sample size
var.test(coast$pcenergy, ncoast$pcenergy) # p-value = 0.5098 (greater than .05), so equal variances can be assumed

t.test(coast[,"pcenergy"], ncoast[,"pcenergy"], alternative= c("two.sided"), paired = FALSE, var.equal= TRUE)
#p-value = 0.2179 (is greater than .05), therefore we cannot reject the null hypothesis that there is on difference in per capita energy consumption between coastal and non-coatal states  

#Question 2

hist(ncoast[,"pccoal"], breaks=15, col="light green", xlim=c(0, 1), ylim=c(0,10), xlab="", main="")
abline(v=mean(ncoast[,"pccoal"]), col="dark green")
par(new=TRUE)
hist(coast[,"pccoal"], breaks=15, col="light blue", xlim=c(0, 1), ylim=c(0,10), xlab="per capita coal consumption", main="")
abline(v=mean(coast[,"pccoal"]), col="blue")
legend("topright", legend = c("Coastal state mean", "Non-Coastal state mean"),text.col=c("blue", "dark green"))

shapiro.test(edata[,"pccoal"])#p-value = 6.024e-11 (smaller than .05) so the distribution is not normal 
var.test(coast$pccoal, ncoast$pccoal) #p-value = 5.995e-13 (smaller than .05) so equal variances cannot be assumed 

t.test (coast[,"pccoal"], ncoast["pccoal"], alternative= c("two.sided"), paired=FALSE, var.equal=FALSE)
#p-value = 0.001936 (less than .05), therefore we can reject the null hypothesis that there is no difference in per capital coal consumption between coastal and non-coastal states. 

#Question 3 
library(car)
boxplot(pccoal~Region, data=edata, xlab="Region", ylab="Per Capita Coal Consumption")

shapiro.test(edata[,"pccoal"]) #p-value = 6.024e-11 (less than .05), so the distribution of the response variable is not normal
qqPlot(edata$pccoal) #not normal 

leveneTest(pccoal~Region, data=edata) #p-value=0.5202 (greater than .05), so equal variances can be assumed 

edata.aov = aov(pccoal~Region, data=edata)
summary(edata.aov)
#p-value =0.262 (greater than .05), therefore there is not a significant difference between state region and per capita coal consumption.

#Question 4

cor(edata$pccoal, edata$pcGDP)
#[1] 0.03598182

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

cor(hdata[,c(1:13)], use="na.or.complete")
#I need to select three that aren't highly correlated, and knew I wanted to do nox because I do air pollution research, so there other two varibles I chose were proportion of blacks (correlation with nox: -0.3584331) and teacher to pupil ratio (correlation with nox: 0.1034642 and correlation with b: -0.08960928)

mod=lm(medv~nox + ptratio + b, data=hdata)
summary(mod)

#Question 5
cor(hdata$nox, hdata$ptratio)
cor(hdata$nox, hdata$b)
cor(hdata$ptratio, hdata$b)

vif.nox = 1/(1 - summary(lm(nox~ptratio+b, data=hdata))$r.squared)
vif.nox #1.154209

vif.b = 1/(1 - summary(lm(b~ptratio+nox, data=hdata))$r.squared)
vif.b #1.151096

vif.pt = 1/(1 - summary(lm(ptratio~b+nox, data=hdata))$r.squared)
vif.pt #1.014065


#Question 6
plot(hdata$nox, hdata$medv)
plot(hdata$b, hdata$medv)
plot(hdata$ptratio, hdata$medv)

#Question 7
mod=lm(medv~nox + ptratio + b, data=hdata)
summary(mod)

plot(residuals(mod)) #my reaction: oh god -- definitely looks like there's a trend

#residual independency
library(lmtest)
dwtest(mod)
#p value < 2.2e-16

#residual homoscedasticity
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red") #looks like they *may* be homoscedastic

bptest(mod)

#residual normality
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")

shapiro.test(residuals(mod))


#Question 8
mod=lm(medv~nox + ptratio + b, data=hdata)
summary(mod)
