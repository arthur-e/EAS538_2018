setwd("D:/UM/SNRE/Winter 2017/NRE 538 Statistics/Final Exam")

#Flying
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
library(dplyr)
#Clearing Data
fly=flying %>%
  subset(select=c(gender,unruly_child))%>%
   subset(is.na(gender)==FALSE) %>%
    subset(is.na(unruly_child)==FALSE)
#Chi-square test
table=table(fly$gender,fly$unruly_child)
table
chisq.test(table)
#################################################
#College
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
#check assumptions
hist(college$tuition)
qqnorm(college$tuition)
qqline(college$tuition,col="red")
shapiro.test(college$tuition)
var.test(tuition~type,data=college)

#test
boxplot(tuition~type,data=college)
t.test(tuition~type,data=college)
##############################################
#Happy
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
#Question 3
#Check Assumptions
hist(happy$Hscore)
qqnorm(happy$Hscore)
qqline(happy$Hscore,col="red")
shapiro.test(happy$Hscore)

library(car)
leveneTest(Hscore~Region,data=happy)

#test
boxplot(Hscore~Region,data=happy)
happyregion=aov(Hscore~Region,data=happy)
summary(happyregion)
TukeyHSD(happyregion)

#Question 4
pairs(happy[,c(4:10)])
cor(happy[,c(4:10)])
hist(happy$Corruption)
qqnorm(happy$Corruption)
qqline(happy$Corruption,col="red")

#Transformation
happy1=happy%>%
  mutate(Corruptionlog=log(Corruption))

cor(happy1[,c(4:11)])
#Model-Without transformation
modelcor=lm(Corruption~GDP+Freedom+Generosity,data=happy1)
summary(modelcor)

#Check Assumptions
rescor=residuals(modelcor)
plot(rescor)
library(lmtest)
dwtest(modelcor,alternative = c("two.sided"))

plot(rescor~fitted(modelcor))
abline(lm(rescor~fitted(modelcor)),col="red")
bptest(modelcor)

hist(rescor)
qqnorm(rescor)
qqline(rescor,col="red")
shapiro.test(rescor)

#Model--transformed dependent variable
modelcorlog=lm(Corruptionlog~GDP+Freedom+Generosity,data=happy1)
summary(modelcorlog)
#Check Assumptions
rescorlog=residuals(modelcorlog)
plot(rescorlog)
dwtest(modelcorlog,alternative = c("two.sided"))

plot(rescorlog~fitted(modelcor))
abline(lm(rescorlog~fitted(modelcorlog)),col="red")
bptest(modelcorlog)

hist(rescorlog)
qqnorm(rescorlog)
qqline(rescorlog,col="red")
shapiro.test(rescorlog)

#Question 5
modelcorre=lm(Corruptionlog~Freedom*Region,data=happy1)
summary(modelcorre)
#Check assumptions
rescorre=residuals(modelcorre)
plot(rescorre)
dwtest(modelcorre,alternative = c("two.sided"))

plot(rescorre~fitted(modelcorre))
abline(lm(rescorre~fitted(modelcorre)),col="red")
bptest(modelcorre)

hist(rescorre)
qqnorm(rescorre)
qqline(rescorre,col="red")
shapiro.test(rescorre)
##############################################
#Cancer
#Question 6
cancer=read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
pairs(cancer)
cor(cancer)
modelcancer=glm(malignant~radius_mean+texture_mean+smoothness_mean,data=cancer,family=binomial(link="logit"))
summary(modelcancer)

#smoothness*100 instead of smoothness_mean
cancer1=cancer%>%
  mutate(smoothness=smoothness_mean*100)
modelcancer1=glm(malignant~radius_mean+texture_mean+smoothness,data=cancer1,family=binomial(link="logit"))
summary(modelcancer1)

dwtest(modelcancer,alternative = c("two.sided"))

#Model fit
mod0=glm(malignant~1, data=cancer, family=binomial(link=logit))
anova(mod0,modelcancer,test="Chi")

#Question 7
anova(modelcancer)
