# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(edata)
str(edata)

# Q1-b
edata.coast0.1=subset(edata,Coast=="0")
edata.coast0.2=edata.coast0.1$TotalEnergy/edata.coast0.1$Population
edata.coast0=cbind(edata.coast0.1,edata.coast0.2)
colnames(edata.coast0)[c(8)]="PerCapitaEnergy"

edata.coast1.1=subset(edata,Coast=="1")
edata.coast1.2=edata.coast1.1$TotalEnergy/edata.coast1.1$Population
edata.coast1=cbind(edata.coast1.1,edata.coast1.2)
colnames(edata.coast1)[c(8)]="PerCapitaEnergy"

hist(edata.coast0[,"PerCapitaEnergy"],breaks=15,col="light green",xlim=c(0,1),ylim=c(0,9),xlab="Per Capita Energy",main="Energy Consumption of States")
abline(v=mean(edata.coast0[,"PerCapitaEnergy"]),col="green")
par(new=TRUE)
hist(edata.coast1[,"PerCapitaEnergy"],breaks=15,col="light blue",xlim=c(0,1),ylim=c(0,9),xlab="",main="")
abline(v=mean(edata.coast1[,"PerCapitaEnergy"]),col="blue")
legend("topright",legend=c("On the coast mean","Not on the coast mean"),text.col=c("dark green","blue"))

# Q1-c
## Test normality
shapiro.test(edata.coast0[,"PerCapitaEnergy"]) # p-value=3.627e-05
shapiro.test(edata.coast1[,"PerCapitaEnergy"]) # p-value=5.037e-06
### Both samples are not normally distrubuted
### => Transform data (log)
edata.coast0.3=log(edata.coast0$PerCapitaEnergy)
edata.coast1.3=log(edata.coast1$PerCapitaEnergy)
shapiro.test(edata.coast0.3) # p-value=0.07863 > 0.05
shapiro.test(edata.coast1.3) # p-value=0.001413
### Samples of states on the coast are still not normally distributed

### Box Cox Power Transformation
library(car)
powerTransform(edata.coast0[,"PerCapitaEnergy"],family="bcPower") # Lambda= -0.4859927
powerTransform(edata.coast1[,"PerCapitaEnergy"],family="bcPower") # Lambda= -1.574563
#### Based on box cox transformation table, use 1/Y transformation
edata.coast0.4=1/edata.coast0$PerCapitaEnergy
edata.coast1.4=1/edata.coast1$PerCapitaEnergy
shapiro.test(edata.coast0.4) # p-value=0.09191 >0.05
shapiro.test(edata.coast1.4) # p-value=0.167 >0.05
#### Both transformed samples are normally distributed

## Test equal variance
var.test(edata.coast0.4,edata.coast1.4)
### p-value=0.3544
### Both transformed samples have equal variance

## T-test
t.test(edata.coast0.4,edata.coast1.4,var.equal=TRUE)
# p-value=0.02006 >0.05
### fail to reject the null hypothesis


# Q2-b
edata.coast0.1=subset(edata,Coast=="0")
edata.coast0.2=edata.coast0.1$TotalCoal/edata.coast0.1$Population
edata.coast0=cbind(edata.coast0.1,edata.coast0.2)
colnames(edata.coast0)[c(8)]="PerCapitaCoal"

edata.coast1.1=subset(edata,Coast=="1")
edata.coast1.2=edata.coast1.1$TotalCoal/edata.coast1.1$Population
edata.coast1=cbind(edata.coast1.1,edata.coast1.2)
colnames(edata.coast1)[c(8)]="PerCapitaCoal"

hist(edata.coast0[,"PerCapitaCoal"],breaks=15,col="light green",xlim=c(0,1),ylim=c(0,10),xlab="Per Capita Coal",main="Coal Consumption of States")
abline(v=mean(edata.coast0[,"PerCapitaCoal"]),col="green")
par(new=TRUE)
hist(edata.coast1[,"PerCapitaCoal"],breaks=15,col="light blue",xlim=c(0,1),ylim=c(0,10),xlab="",main="")
abline(v=mean(edata.coast1[,"PerCapitaCoal"]),col="blue")
legend("topright",legend=c("On the coast mean","Not on the coast mean"),text.col=c("dark green","blue"))

# Q2-c
## Test normality
shapiro.test(edata.coast0[,"PerCapitaCoal"]) # p-value=6.211e-07
shapiro.test(edata.coast1[,"PerCapitaCoal"]) # p-value=0.001023
### Both samples are not normally distrubuted
### => Transform data (square root)
edata.coast0.3=sqrt(edata.coast0$PerCapitaCoal)
edata.coast1.3=sqrt(edata.coast1$PerCapitaCoal)
shapiro.test(edata.coast0.3) # p-value=0.008021
shapiro.test(edata.coast1.3) # p-value=0.4488 >0.05
### Samples of states on the coast become normally distributed
### Samples of states not on the coast still do not pass shapiro test, but they are far better than no transformed ones

## Test equal variance
var.test(edata.coast0.3,edata.coast1.3)
### p-value=0.000254
### Both samples do not have equal variance
### So we will use Welch's t-test

## T-test
t.test(edata.coast0.3,edata.coast1.3)
# p-value=4.512e-05 <0.05
### reject the null hypothesis

## Wilcoxon-Mann-Whitney U test (non-parametric test)
edata.coast10=edata$TotalCoal/edata$Population
edata.coast=cbind(edata,edata.coast10)
colnames(edata.coast)[c(8)]="PerCapitaCoal"
head(edata.coast)
wilcox.test(PerCapitaCoal~Coast,data=edata.coast)
# p-value=2.051e-05 <0.05
### reject the null hypothesis


# Q3-b
edata.south1=subset(edata,Region=="South")
edata.south2=edata.south1$TotalCoal/edata.south1$Population
edata.south=cbind(edata.south1,edata.south2)
colnames(edata.south)[c(8)]="PerCapitaCoal"

edata.west1=subset(edata,Region=="West")
edata.west2=edata.west1$TotalCoal/edata.west1$Population
edata.west=cbind(edata.west1,edata.west2)
colnames(edata.west)[c(8)]="PerCapitaCoal"

edata.midwest1=subset(edata,Region=="Midwest")
edata.midwest2=edata.midwest1$TotalCoal/edata.midwest1$Population
edata.midwest=cbind(edata.midwest1,edata.midwest2)
colnames(edata.midwest)[c(8)]="PerCapitaCoal"

edata.east1=subset(edata,Region=="East")
edata.east2=edata.east1$TotalCoal/edata.east1$Population
edata.east=cbind(edata.east1,edata.east2)
colnames(edata.east)[c(8)]="PerCapitaCoal"

hist(edata.south[,"PerCapitaCoal"],breaks=15,col="light green",xlim=c(0,1),ylim=c(0,8),xlab="Per Capita Coal",main="Coal Consumption of Regions")
abline(v=mean(edata.south[,"PerCapitaCoal"]),col="green")
par(new=TRUE)
hist(edata.midwest[,"PerCapitaCoal"],breaks=15,col="pink",xlim=c(0,1),ylim=c(0,8),xlab="Per Capita Coal",main="Coal Consumption of Regions")
abline(v=mean(edata.midwest[,"PerCapitaCoal"]),col="pink")
par(new=TRUE)
hist(edata.west[,"PerCapitaCoal"],breaks=15,col="light blue",xlim=c(0,1),ylim=c(0,8),xlab="",main="")
abline(v=mean(edata.west[,"PerCapitaCoal"]),col="blue")
par(new=TRUE)
hist(edata.east[,"PerCapitaCoal"],breaks=15,col="yellow",xlim=c(0,1),ylim=c(0,8),xlab="",main="")
abline(v=mean(edata.east[,"PerCapitaCoal"]),col="orange")
legend("topright",legend=c("South mean","Midwest mean","West mean","East mean"),text.col=c("dark green","red","blue","orange"))

# Q3-c
## Test normality
shapiro.test(edata.south[,"PerCapitaCoal"])   # p-value=5.689e-05
shapiro.test(edata.west[,"PerCapitaCoal"])    # p-value=1.103e-05
shapiro.test(edata.midwest[,"PerCapitaCoal"]) # p-value=0.0001758
shapiro.test(edata.east[,"PerCapitaCoal"])    # p-value=5.535e-06
### All samples are not normally distributed
### => Transform data (square root)
edata.south3=sqrt(edata.south[,"PerCapitaCoal"])
shapiro.test(edata.south3)   # p-value=0.06557 >0.05
edata.west3=sqrt(edata.west[,"PerCapitaCoal"])
shapiro.test(edata.west3)    # p-value=0.003132
edata.midwest3=sqrt(edata.midwest[,"PerCapitaCoal"])
shapiro.test(edata.midwest3) # p-value=0.006807
edata.east3=sqrt(edata.east[,"PerCapitaCoal"])
shapiro.test(edata.east3)    # p-value=0.003827
### Samples of the south region become normally distributed
### Samples of the other three still do not pass shapiro test, but they are far better than no transformed ones

## Test equal variance
var.test(edata.south3,edata.west3)   # p-value=0.08855 >0.05
var.test(edata.west3,edata.midwest3) # p-value=0.1044 >0.05
var.test(edata.midwest3,edata.east3) # p-value=0.1754 >0.05

edata.region1=sqrt(edata$TotalCoal/edata$Population)
edata.region=cbind(edata,edata.region1)
colnames(edata.region)[c(8)]="PerCapitaCoal"
head(edata.region)
levene.test(PerCapitaCoal~Region,data=edata.region) # p-value=0.3803 >0.05
### All samples have equal variance

## ANOVA
summary(aov(PerCapitaCoal~Region,data=edata.region))
### p-value=0.00542 <0.05
### reject the null hypothesis

## Kruskal Wallis test (non-parametric test)
edata.region2=edata$TotalCoal/edata$Population
edata.region3=cbind(edata,edata.region2)
colnames(edata.region3)[c(8)]="PerCapitaCoal"
head(edata.region3)
kruskal.test(PerCapitaCoal~Region,data=edata.region3)
### p-value=0.00042 <0.05
### reject the null hypothesis


# Q4
edata.coal=edata$TotalCoal/edata$Population
edata.GDP=edata$TotalGDP/edata$Population
edata.coalGDP=cbind(edata,edata.coal,edata.GDP)
colnames(edata.coalGDP)[c(8:9)]=c("PerCapitaCoal","PerCapitaGDP")
head(edata.coalGDP)

## Correlation
plot(edata.coalGDP$PerCapitaCoal,edata.coalGDP$PerCapitaGDP,xlab="Per Capita Coal",ylab="Per Capita GDP",main="Correlation between Coal Consumption and GDP")
abline(lm(PerCapitaCoal~PerCapitaGDP,data=edata.coalGDP),col="red")
cor(edata.coalGDP$PerCapitaCoal,edata.coalGDP$PerCapitaGDP)
### r=0.03598182



# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(hdata)
str(hdata)

# Q5
cor(hdata[,c(1,6,7)]) # cor < 0.5
library(fmsb)
VIF(lm(medv~crim+rm+age,data=hdata)) # VIF=2.439356

# Q6
cor(hdata[,c(1,6,7,14)])
plot(medv~crim,data=hdata,main="The relathioship beween crim and medv")
abline(lm(medv~crim,data=hdata),col="red")
legend("right", legend = c(paste0("r = ",-0.286)),text.col=c("blue"))
plot(medv~rm,data=hdata,main="The relathioship beween rm and medv")
abline(lm(medv~rm,data=hdata),col="red")
legend("topleft", legend = c(paste0("r= ",-0.74)),text.col=c("blue"))
plot(medv~age,data=hdata,main="The relathioship beween age and medv")
abline(lm(medv~age,data=hdata),col="red")
legend("bottomleft", legend = c(paste0("r = ",-0.3)),text.col=c("blue"))


# Q7
mod=lm(medv~crim+rm+age,data=hdata)
summary(mod)

## Test residual independency
plot(residuals(mod))
library(lmtest)
dwtest(mod,alternative=c("two.sided")) # p-value=2.2e-16
### The residuals show autocorrelation

## Test residual homoscedasticity
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)),col="red")
bptest(mod) # p-value=8.811e-07
### The residuals are heteroscedatstic

## Test residual normality
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")
shapiro.test(residuals(mod)) # p-value=2.2e-16
### The residuals are not normally distributed


# Q8
mod=lm(medv~crim+rm+age,data=hdata)
summary(mod)
mod.intr=lm(medv~crim+rm*age,data=hdata)
summary(mod.intr)




########################
## Resolve the violated assumptions
library(car)
powerTransform(hdata[,"crim"],family="bcPower") # Lambda= -0.131
powerTransform(hdata[,"rm"],family="bcPower") # Lambda= -0.325
powerTransform(hdata[,"age"],family="bcPower") # Lambda= 1.18

### Data transformation
hdata.tsfm1=1/sqrt(hdata$crim)
hdata.tsfm2=1/sqrt(hdata$rm)
hdata.tsfm3=1/sqrt(hdata$age)
hdata.tsfm4=hdata$medv
hdata.tsfm5=cbind(hdata.tsfm1,hdata.tsfm2,hdata.tsfm3,hdata.tsfm4)
colnames(hdata.tsfm5)[c(1:4)]=c("crim","rm","age","medv")
head(hdata.tsfm5)
hdata.tsfm=as.data.frame(hdata.tsfm5)
mod.tsfm=lm(medv~crim+rm+age,data=hdata.tsfm)
summary(mod.tsfm)

### Identify outliers
plot(mod)
hdata.remv=subset(hdata[-c(225,347,448,449,450,451,452),])
str(hdata.remv) # obs: hdata 452 => hdata.remv 445
mod.remv=lm(medv~crim+rm+age,data=hdata.remv)
summary(mod.remv)


#### Test assumptions
# Outliers removed
## Test residual independency
plot(residuals(mod.remv))
library(lmtest)
dwtest(mod.remv,alternative=c("two.sided")) # p-value=2e-07
### The residuals show autocorrelation

## Test residual homoscedasticity
plot(residuals(mod.remv)~fitted(mod.remv))
abline(lm(residuals(mod.remv)~fitted(mod.remv)),col="red")
bptest(mod.tsfm) # p-value=9e-04
### The residuals are heteroscedatstic

## Test residual normality
qqnorm(residuals(mod.remv))
qqline(residuals(mod.remv), col="red")
shapiro.test(residuals(mod.tsfm)) # p-value=2e-16
### The residuals are not normally distributed


# Data transformed
## Test residual independency
plot(residuals(mod.tsfm))
library(lmtest)
dwtest(mod.tsfm,alternative=c("two.sided")) # p-value=2e-16
### The residuals show autocorrelation

## Test residual homoscedasticity
plot(residuals(mod.tsfm)~fitted(mod.tsfm))
abline(lm(residuals(mod.tsfm)~fitted(mod.tsfm)),col="red")
bptest(mod.tsfm) # p-value=9e-04
### The residuals are heteroscedatstic

## Test residual normality
qqnorm(residuals(mod.tsfm))
qqline(residuals(mod.tsfm), col="red")
shapiro.test(residuals(mod.tsfm)) # p-value=2e-16
### The residuals are not normally distributed

##################################

head(hdata)
mod.full=lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat,data=hdata)
anova(mod,mod.full)


library(dplyr)
hdata.mu=mutate(hdata,crim=(crim-mean(hdata$crim))/sd(hdata$crim),rm=(rm-mean(hdata$rm))/sd(hdata$rm),age=(age-mean(hdata$age))/sd(hdata$age))
mod.mu=lm(medv~crim+rm+age,data=hdata.mu)
summary(mod.mu)

