# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

##1b
library(reshape2)
library(plyr)
library(magrittr)
edata = edata %>%
  mutate(pc.e = (TotalEnergy/Population))

coastal=subset(edata, Coast==1)
noncoast=subset(edata, Coast==0)

hist(noncoast$pc.e, col=rgb(1,0,0,0.5), 
     main="Energy Use Per Capita in Coast and Noncoastal States",
     xlab="Energy Use Per Capita")
hist(coastal$pc.e, col=rgb(0,0,1,0.5), add=T)
##1c
var.test(coastal$pc.e, noncoast$pc.e)
shapiro.test(coastal$pc.e)
shapiro.test(noncoast$pc.e)
qqnorm(coastal$pc.e)
qqline(coastal$pc.e)
qqnorm(noncoast$pc.e)
qqline(noncoast$pc.e)

##1d
t.test(coastal$pc.e,noncoast$pc.e, var.equal=TRUE, paired=FALSE)


##2b
coastal=subset(edata, Coast==1)
noncoast=subset(edata, Coast==0)
coast.coal=coastal$TotalCoal/coastal$Population
noncoast.coal=noncoast$TotalCoal/noncoast$Population

hist(noncoast.coal, col=rgb(1,0,0,0.5), 
     main="Coal Use Per Capita in Coast and Noncoastal States",
     xlab="Coal Use Per Capita")
hist(coast.coal, col=rgb(0,0,1,0.5), add=T)

##2c
var.test(coast.coal,noncoast.coal)
shapiro.test(coast.coal)
qqnorm(coast.coal)
shapiro.test(noncoast.coal)
qqnorm(noncoast.coal)


##2d
t.test(coast.coal,noncoast.coal, paired=FALSE)
t.test(coast.coal,noncoast.coal, paired=FALSE,
       alternative = "greater")

##3b
boxplot((edata$TotalCoal/edata$Population)~Region, data=edata)
South=subset(edata, Region=="South")
West=subset(edata, Region=="West")
Midwest=subset(edata, Region=="Midwest")
East=subset(edata, Region=="East")

sswfun = function(x){
  n = length(x)
  s2 = var(x)
  ssw = (n-1)*s2
  return(ssw)}

Southw = sswfun(South$TotalCoal/South$Population)
Westw = sswfun(West$TotalCoal/West$Population)
Midwestw = sswfun(Midwest$TotalCoal/Midwest$Population)
Eastw= sswfun(East$TotalCoal/East$Population)
ssw = sum(Southw,Westw,Midwestw,Eastw)

ssbfun = function(x){
  n = length(x)
  x = mean(x)
  gx = mean(c(South$TotalCoal/South$Population,
              West$TotalCoal/West$Population,
              Midwest$TotalCoal/Midwest$Population,
              East$TotalCoal/East$Population))
  ssb = n*(x-gx)^2
  return(ssb)}

Southb = ssbfun(South$TotalCoal/South$Population)
Westb = ssbfun(West$TotalCoal/West$Population)
Midwestb = ssbfun(Midwest$TotalCoal/Midwest$Population)
Eastb= ssbfun(East$TotalCoal/East$Population)
ssb = sum(Southw,Westw,Midwestw,Eastw)

sst = ssb + ssw

msb = ssb/(4-1)
msw = ssw/(51-4)

fstat = msb/msw
fcrit = qf(0.95,df1=3,df2=47)
pvalue = 1 - pf(fstat,df1=3,df2=47)

curve(df(x,df1=3,df2=47), xlim=c(0,40), xlab='',ylab='')
abline(v=fcrit,col='gray')
abline(v=fstat,col='red')

##3c
library(car)

leveneTest((edata$TotalCoal/edata$Population)~ Region, data=edata)

shapiro.test(South$TotalCoal/South$Population)
shapiro.test(West$TotalCoal/West$Population)
shapiro.test(Midwest$TotalCoal/Midwest$Population)
shapiro.test(East$TotalCoal/East$Population)

tran.1=log(South$TotalCoal/South$Population)
shapiro.test(tran.1)
tran.2=sqrt(South$TotalCoal/South$Population)
shapiro.test(tran.2)

tran.3=sqrt(West$TotalCoal/West$Population)
shapiro.test(tran.3)

tran.4=log10(South$TotalCoal/South$Population)
shapiro.test(tran.4)

##3d
coal.mod=(aov((edata$TotalCoal/edata$Population)~Region, data=edata))
summary(coal.mod)


##4
cor((edata$TotalCoal/edata$Population), (edata$TotalGDP/edata$Population))

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)
##5
cor(hdata[,c(1,6,8)], use="na.or.complete")
h.mod=lm(medv~crim+rm+dis, data=hdata)
vif(h.mod)

##6
plot(medv~crim, data=hdata)
plot(medv~rm, data=hdata)
plot(medv~dis, data=hdata)

##7
h.mod=lm(medv~crim+rm+dis, data=hdata)
plot(residuals(h.mod))
library(lmtest)
dwtest(h.mod, alternative=c("two.sided"))

qqnorm(residuals(h.mod))
qqline(residuals(h.mod), col="red")
shapiro.test(residuals(h.mod))

plot(residuals(h.mod)~fitted(h.mod))
abline(lm(residuals(h.mod)~fitted(h.mod)), col="red")
library(lmtest)
bptest(h.mod)

##8
h.mod=lm(medv~crim+rm+dis, data=hdata)
summary(h.mod)
h.mod2=lm(medv~crim+rm+dis+rm*dis, data=hdata)
summary(h.mod2)

