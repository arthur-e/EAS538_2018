flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

library(RCurl)
library(dplyr)
library(ggplot2)
library(magrittr)
library(reshape2)
library(plyr)

#############Problem 1###############

head(flying)

data=flying %>%
  select(c(2,19))
head(data)

male=subset(dat, dat$gender=="Male")
head(male)
female=subset(dat, dat$gender=="Female")
head(female)

##Assumptions##

dat=na.omit(data)
head(dat)

chisq.test(male, female)


#Independent Observations


##Run the Test##
str(dat)

fly.1 = fly %>%
  mutate(as.numeric(dat$gender)) %>%
  mutate(as.numeric(dat$unruly_child))

set.seed(1) 
dat$scode[dat$gender=="Male"]<-"1" 
dat$scode[dat$gender=="Female"]<-"0" 
dat$scode<-as.factor(dat$scode) 
head(dat) 

set.seed(1) 
dat$childcode[dat$unruly_child=="No"]<-"1" 
dat$childcode[dat$unruly_child=="Somewhat"]<-"3" 
dat$childcode[dat$unruly_child=="Very"]<-"5" 
dat$childcode<-as.factor(dat$childcode) 
head(dat) 

fly=dat %>%
  select(c(3,4))
head(fly)

boxplot(fly$childcode, xlab="scode", ylab="unruly child (effect size)")

plot(fly)

male=subset(fly.1, fly$scode=="0")
head(male)

female=subset(fly.1, fly$scode=="1")
head(female)

t.test(male, female)

summary(fly)

#############Problem 2################

public=subset(college$tuition, college$type=="Public")
head(public)
summary(public)

private=subset(college$tuition, college$type=="Private nonprofit")
head(private)
summary(private)

##Visualize##
hist(public, breaks=30, col="light blue", xlim=c(0, 50000), ylim=c(0,100), xlab="tuition", main="")
abline(v=mean(public), col="blue")
par(new=TRUE)
hist(private, breaks=50, col="light green", xlim=c(0, 50000), ylim=c(0,100), xlab="tuition", main="")
abline(v=mean(private), col="dark green")
legend("topright", legend = c("Public mean", "Private mean"),text.col=c("blue", "dark green"))

boxplot(college$tuition~college$type, xlab="type", ylab="Type (effect size)")

plot(private)
plot(public)

##Assumptions##

#Normality#
shapiro.test(college$tuition)

qqnorm(college$tuition)
qqline(college$tuition, col="red")

#Variance#
var.test(public, private)

##Test##
t.test(public, private, paired=FALSE, var.equal=FALSE)




#############Problem 3################

Happiness_Africa=subset(happy$Hscore, happy$Region=="AfricaMideast")
plot(Happiness_Africa)

Happiness_Americas=subset(happy$Hscore, happy$Region=="AmericasCarribean")
plot(Happiness_Americas)

Happiness_Asia=subset(happy$Hscore, happy$Region=="AsiaAustralia")
plot(Happiness_Asia)

Happiness_Europe=subset(happy$Hscore, happy$Region=="Europe")
plot(Happiness_Europe)

##Visualize##

boxplot(happy$Hscore~happy$Region, xlab="region", ylab="Region (effect size)")

##Assumptions##

#Normality#
shapiro.test(happy$Hscore)

qqnorm(happy$Hscore)
qqline(happy$Hscore, col="red")

#Variance#
library(Rcmdr)
leveneTest(happy$Hscore~happy$Region)

##Test##
aov=aov(happy$Hscore~happy$Region)
summary(aov)
TukeyHSD(aov)

lm=lm(happy$Hscore~happy$Region)
summary(lm)



#############Problem 4#############

##Visualize##
plot(happy$Corruption)
qqnorm(happy$Corruption)
qqline(happy$Corruption, col="red")

corrupt.log = log(happy$Corruption)

qqnorm(corrupt.log)
qqline(corrupt.log, col="red")

##Run the test##

mod=lm(corrupt.log~happy$Hscore + happy$GDP + happy$Generosity)
summary(mod)

mod.1=lm(corrupt.log~happy$Hscore + happy$Freedom + happy$Generosity)
summary(mod.1)


exp(-3.6062)
exp(0.2614)

exp(-3.35861)

##Assumptions##

#Correlation#
pairs(happy[, c("Corruption", "Hscore", "Freedom", "Generosity")])
cor(happy[, c("Corruption", "Hscore", "Freedom", "Generosity")], use="na.or.complete")

#Independent errors#
plot(residuals(mod.1))

library(lmtest)
dwtest(mod.1, alternative=c("two.sided"))

#Homoscedasticity#

plot(residuals(mod.1)~fitted(mod.1))
abline(lm(residuals(mod.1)~fitted(mod.1)), col="red")

bptest(mod.1)

#Normal errors#

qqnorm(residuals(mod.1))
qqline(residuals(mod.1), col="red")

shapiro.test(residuals(mod.1))



#############Problem 5##############

library(Lahman)
library(dplyr)
library(ggplot2)

##Running the test##
mod.2 = lm(corrupt.log~happy$Freedom*happy$Region)
summary(mod.2)




#############Problem 6##############

head(cancer)

mod.3 = glm(cancer$malignant~cancer$texture_mean + cancer$area_mean + cancer$smoothness_mean,family=binomial(link="logit")) 
summary(mod.3)

exp(-32.619261) #intercept 6.817654e-15
exp(0.381076) # texture 1.463859
exp(0.016260) # area 1.016393
exp(146.766602) # smoothness 5.494465e+63


##Model Checking##

mod.4 = glm(cancer$malignant~cancer$texture_mean + cancer$area_mean + cancer$smoothness_mean + cancer$radius_mean + cancer$perimeter_mean,family=binomial(link="logit")) 
summary(mod.4)

mod.5 = glm(cancer$malignant~cancer$texture_mean,family=binomial(link="logit"))
summary(mod.5)

anova(mod.3, mod.4, mod.5, test="Chi")

AIC(mod.3, mod.4, mod.5)

plot(residuals(mod.3))

##############Bonus Question##############

mod.6 = glm(cancer$malignant~cancer$texture_mean,family=binomial(link="logit"))
mod.7 = glm(cancer$malignant~cancer$area_mean,family=binomial(link="logit"))
mod.8 = glm(cancer$malignant~cancer$smoothness_mean,family=binomial(link="logit"))

anova(mod.6, mod.7, mod.8, test="Chi")

AIC(mod.6, mod.7, mod.8)

