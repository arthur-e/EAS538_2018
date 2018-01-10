# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

#Q1-1####
coast1=subset(edata,Coast==1)
perenergy1=coast1$TotalEnergy/coast1$Population
coast0=subset(edata,Coast==0)
perenergy0=coast0$TotalEnergy/coast0$Population
shapiro.test(perenergy1)
#p-value = 5.037e-06 not a normally distribution
shapiro.test(perenergy0)
#p-value = 3.627e-05 not a normally distribution
var.test(perenergy1,perenergy0)
#p=0.5098, so the variance is same
wilcox.test(perenergy0,perenergy1,paired = F)
#p-value = 0.008417, it is significant

data3
boxplot(pe~Coast,data=data3, xlab="Coast",ylab="Per Capita Energy")

#Q1-2####
coast1=subset(edata,Coast==1)
coast0=subset(edata,Coast==0)
percoal1=coast1$TotalCoal/coast1$Population
percoal0=coast0$TotalCoal/coast0$Population
var.test(percoal1,percoal0)
#p-value=5.995e-13.
shapiro.test(percoal1)
#p-value = 0.001023,so this is not a normally distribution
shapiro.test(percoal0)
#p-value = 6.211e-07,so this is not a normally distribution
wilcox.test(percoal0,percoal1)
#?ties in the two 
boxplot(pc~Coast,data=data3,xlab="Coast",ylab="Per Capita Coal")
avg.m=c()
for (i in 1:10000){
  avg.m[i]=mean(sample(percoal0, length(percoal0), replace=TRUE))
}

av1.m=c()
for (i in 1:10000){
  av1.m[i]=mean(sample(percoal1, length(percoal1), replace=TRUE))
}

t.test(avg.m,av1.m)

#Q1-3####
pc=edata$TotalCoal/edata$Population
data1=cbind(edata,pc)
shapiro.test(data1$pc)
#p-value=6.024e-11, not normally distributed
kruskal.test(pc~Region,data=data1)
boxplot(pc~Region,data=data3,xlab="Region",ylab="Per Capita Coal")

#Q1-4####
pe=edata$TotalEnergy/edata$Population
data2=cbind(data1,pe)
pgdp=edata$TotalGDP/edata$Population
data3=cbind(data2,pgdp)
c=cor(data3[, c("pc", "pgdp")], use="na.or.complete")
c


# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
#Q2-5####
mod=lm(medv~crim+ptratio+lstat,data=hdata)
summary(mod)
cor1=cor(hdata$crim,hdata$ptratio)
cor1
cor2=cor(hdata$crim,hdata$lstat)
cor2
cor3=cor(hdata$ptratio,hdata$lstat)
cor3
vif1=1/(1-summary(lm(crim~ptratio+lstat,data=hdata))$r.squared)
vif1
vif2=1/(1-summary(lm(ptratio~crim+lstat,data=hdata))$r.squared)
vif2
vif3=1/(1-summary(lm(lstat~crim+ptratio,data=hdata))$r.squared)
vif3

#Q2-6####
mod1=lm(medv~crim,data=hdata)
summary(mod1)
plot(medv~crim,data=hdata)
abline(mod1,col="red")
mod2=lm(medv~ptratio,data=hdata)
summary(mod2)
plot(medv~ptratio,data=hdata)
abline(mod2,col="red")
mod3=lm(medv~lstat,data=hdata)
summary(mod3)
plot(medv~lstat,data=hdata)
abline(mod3,col="red")

#Q2-7####
library(lmtest)
mod=lm(medv~crim+ptratio+lstat,data=hdata)
summary(mod)
shapiro.test(residuals(mod))
dwtest(mod, alternative=c("two.sided"))
bptest(mod)
shapiro.test(hdata$medv)
moda=lm(log(medv)~crim+ptratio+lstat,data=hdata)
plot(lm(log(medv)~crim+ptratio+lstat,data=hdata))
shapiro.test(residuals(moda))
modb=lm(sqrt(medv)~crim+ptratio+lstat,data=hdata)
plot(modb)
shapiro.test(residuals(modb))
modc=lm((1/medv)~crim+ptratio+lstat,data=hdata)
shapiro.test(residuals(modc))
qqplot(modc)

modd=lm(exp(medv)~crim+ptratio+lstat,data=hdata)
shapiro.test(residuals(modd))

#Q2-8,9####
mod=lm(medv~crim+ptratio+lstat,data=hdata)
summary(mod)
model1=lm(medv~crim+lstat*ptratio,data=hdata)
summary(model1)
