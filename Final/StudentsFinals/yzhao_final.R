library(car)
library(plyr)
library(reshape2)
library(magrittr)
library(RCurl)
library(ggplot2)

#Q1
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
chisq.test(flying$gender,flying$unruly_child) # p-value = 0.001193<0.05,so reject null hypothesis
plot(flying$gender,flying$unruly_child,xlab="Gender",ylab="Attitudes toward Bringing An Unruly Child on plane")

#Q2
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
qqnorm(college$tuition)
qqline(college$tuition,col="red")
shapiro.test(college$tuition) #3.193e-15, not normally distributed
public=subset(college,type=="Public")
private=subset(college,type=="Private nonprofit")
var.test(public$tuition,private$tuition) #p-value < 2.2e-16, variance are different
t.test(public$tuition,private$tuition) #p-value < 2.2e-16
mean(public$tuition)
mean(private$tuition)
boxplot(college$tuition~college$type,xlab="school type", ylab="tuition")

#Q3
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
shapiro.test(happy$Hscore) #p-value = 0.01248,not normal distributed
leveneTest(Hscore~Region,data=happy) # p-value = 0.5179, the variance are equal
kruskal.test(Hscore~Region,data=happy) #p-value=3.841e-11, so reject null hypothesis.

#Q4
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
## 1. Test linear relationship
pairs(happy[, c("Corruption", "GDP", "Freedom", "Family")])
cor(happy[, c("Corruption","GDP","Freedom","Family")], use="na.or.complete")

## 2. Test normality of the dependent variable
shapiro.test(happy$Corruption) # p-value = 5.128e-11, not normally distributed
qqnorm(happy$Corruption)
qqline(happy$Corruption,col="red")
plot(happy$Corruption)
### log Transform data
happy = happy %>%
  mutate(Corruption=log(Corruption))
### Test normality of the log transform data
shapiro.test(happy$Corruption) # p-value = 0.08535 it is normally distributed 
qqnorm(happy$Corruption)
qqline(happy$Corruption,col="red")

## 3. Test multi-collinearity
cor(happy[, c("GDP","Freedom","Family")], use="na.or.complete")
install.packages("fmsb")
library(fmsb)
VIF(lm(Family~GDP+Freedom,data=happy)) #VIF=1.991691 <10, there is no multi-collinearity
VIF(lm(GDP~Freedom+Family,data=happy)) #VIF=1.827911 <10, there is no multi-collinearity
VIF(lm(Freedom~GDP+Family,data=happy)) #VIF=1.26486 <10, there is no multi-collinearity

## 4. Build a linear model
mod1=lm(Corruption~GDP+Freedom+Family,data=happy)

## 5. check moddel
plot(mod1)
### independency of residuals
plot(residuals(mod1))
library(lmtest)
dwtest(mod1,alternative=c("two.sided")) # p-value = 0.3309 >0.05, residuals are independent
### homoscedasticity of residuals
bptest(mod1) #p-value=0.6294>0.05,homoscedastic
### nomality of residuals
shapiro.test(residuals(mod1)) #p-value = 0.001728<0.05, not normal distributed

## 6. transform data
####QQplot shows outliers of model 1 are 86,15,49
happy2=subset(happy[-c(15,29,86),])
mod2=lm(Corruption~Family+GDP+Freedom,data=happy2)

## 7. check model again
plot(mod2)
### normality of residuals
shapiro.test(residuals(mod2)) #p-value=0.01923, a little bit better than mod1
### independency of residuals
plot(residuals(mod1))
library(lmtest)
dwtest(mod2,alternative=c("two.sided")) #p-value = 0.2425,still independent
### homoscedasticity of residuals
bptest(mod2) #p-value = 0.5149, still homoscedastic

## 8. transform model again
happy3=subset(happy[-c(15,29,86,22,49,156),])
mod3=lm(Corruption~GDP+Freedom+Family,data=happy3)

## 9.check model again
plot(mod3)
### normality of residuals
shapiro.test(residuals(mod3)) #p-value=0.1555, normally distributed
### independency of residuals
plot(residuals(mod3))
library(lmtest)
dwtest(mod3,alternative=c("two.sided")) #p-value = 0.2509,still independent
### homoscedasticity of residuals
bptest(mod3) #p-value = 0.5813, still homoscedastic

## 10.run mod3
mod3=lm(Corruption~Family+GDP+Freedom,data=happy3)
summary(mod3)


#Q5
mod4=lm(Corruption~Freedom*Region,data=happy3)
summary(mod4)

#Q6
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
## multi-collinearity
pairs(cancer[,])
cor(cancer[,], use="na.or.complete")
cor(cancer[, c("area_mean","texture_mean","smoothness_mean")], use="na.or.complete")
install.packages("fmsb")
library(fmsb)
VIF(lm(area_mean~texture_mean+smoothness_mean,data=cancer)) # VIF = 1.158975 < 10,no multi-collinearity
VIF(lm(texture_mean~area_mean+smoothness_mean,data=cancer)) # VIF = 1.123269 < 10,no multi-collinearity
VIF(lm(smoothness_mean~area_mean+texture_mean,data=cancer)) # VIF = 1.040059 < 10, no multi-collinearity
## GLM
cancer$malignant=factor(cancer$malignant)
mod5= glm(malignant~area_mean+texture_mean+smoothness_mean,data=cancer,family='binomial')
summary(mod5)

#Q7
modc1=glm(malignant~area_mean,data=cancer,family='binomial')
modc2=glm(malignant~texture_mean,data=cancer,family='binomial')
modc3=glm(malignant~smoothness_mean,data=cancer,family='binomial')
summary(modc1) #AIC=329.66
summary(modc2) #AIC=650.52
summary(modc3) #AIC=677.95

