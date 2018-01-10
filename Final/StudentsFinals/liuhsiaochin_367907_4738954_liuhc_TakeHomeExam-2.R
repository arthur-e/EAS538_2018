library(car)
library(plyr)
library(reshape2)
library(magrittr)
library(RCurl)
library(ggplot2)

# Q1
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
head(flying)
table(flying$unruly_child)
table(flying$gender)
chisq.test(flying$gender,flying$unruly_child) # p-value = 0.001193 (reject the null hypothesis)
plot(flying$gender,flying$unruly_child,xlab="Gender",ylab="Attitudes toward Bringing An Unruly Child")


# Q2
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
head(college)
table(college$type)

## Test normality
shapiro.test(college$tuition) # p-value = 3.193e-15
qqnorm(college$tuition)
qqline(college$tuition,col="red")
### Transform data
college = college %>%
  mutate(tuition=sqrt(tuition))
### Test normality again
shapiro.test(college$tuition) # p-value = 2.932e-07 (better)
qqnorm(college$tuition)
qqline(college$tuition,col="red")

## Test equal variance
public = subset(college,type=="Public")
private = subset(college,type=="Private nonprofit")
var.test(public$tuition,private$tuition) # p-value = 2.743e-10 (no equal variance)

## Welch's t-test
t.test(public$tuition,private$tuition,var.equal=FALSE) # p-value = 2.2e-16 (reject the null hypothesis)
public.mean = mean(public$tuition) # = 135.4532
private.mean = mean(private$tuition) # = 165.7107

# Q3
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
head(happy)
table(happy$Region)

## Test normality
shapiro.test(happy$Hscore) # p-value = 0.01248
qqnorm(happy$Hscore)
qqline(happy$Hscore,col="red")
### Transform data
happy = happy %>%
  mutate(Hscore=sqrt(Hscore))
### Test normality again
shapiro.test(happy$Hscore) # p-value = 0.0145 (not better)
qqnorm(happy$Hscore)
qqline(happy$Hscore,col="red")
table(happy$Region)
### Use original dataset
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

## Test equal variance
leveneTest(Hscore~Region,data=happy) # p-value = 0.5179 (equal variance)

## ANOVA
summary(aov(Hscore~Region,data=happy)) # p-value = 1.28e-12 (reject the null hypothesis)


# Q4
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
head(happy)

## Test linear relationship
pairs(happy[, c("Corruption", "GDP", "Freedom", "Generosity")])
cor(happy[, c("Corruption","GDP","Freedom","Generosity")], use="na.or.complete")
cor.test(happy$Corruption,happy$GDP) # p-value = 0.0001842 (linear)

## Test normality of the dependent variable
shapiro.test(happy$Corruption) # p-value = 5.128e-11
qqnorm(happy$Corruption)
qqline(happy$Corruption,col="red")
### Transform data
happy = happy %>%
  mutate(Corruption=log(Corruption))
### Test normality again
shapiro.test(happy$Corruption) # p-value = 0.08535 (normal)
qqnorm(happy$Corruption)
qqline(happy$Corruption,col="red")

## Test multi-collinearity
cor(happy[, c("GDP","Freedom","Generosity")], use="na.or.complete")
library(fmsb)
VIF(lm(Corruption~GDP+Freedom,data=happy)) # VIF = 1.254831 < 10 (no multi-collinearity)
1/(1-summary(lm(Corruption~GDP+Freedom,data=happy))$r.square) # = 1.254831
VIF(lm(Corruption~Freedom+Generosity,data=happy)) # VIF = 1.266206 < 10 (no multi-collinearity)
1/(1-summary(lm(Corruption~Freedom+Generosity,data=happy))$r.square) # = 1.266206
VIF(lm(Corruption~GDP+Generosity,data=happy)) # VIF = 1.097842 < 10 (no multi-collinearity)
1/(1-summary(lm(Corruption~GDP+Generosity,data=happy))$r.square) # = 1.097842

mod1=lm(Corruption~GDP+Freedom+Generosity,data=happy)

## Test residual independency
plot(residuals(mod1))
library(lmtest)
dwtest(mod1,alternative=c("two.sided")) # p-value = 0.3701 (independent)

## Test residual homoscedasticity
plot(residuals(mod1)~fitted(mod1))
abline(lm(residuals(mod1)~fitted(mod1)),col="red")
plot(mod1,which=1)
bptest(mod1) # p-value = 0.06878 (homoscedastic)

## Test residual normality
qqnorm(residuals(mod1))
qqline(residuals(mod1), col="red")
plot(mod1,which=2)
shapiro.test(residuals(mod1)) # p-value = 0.0006382
### Remove outliers
happy.remv=subset(happy[-c(22,49,86),])
mod2=lm(Corruption~GDP+Freedom+Generosity,data=happy.remv)
### Test residual normality again
qqnorm(residuals(mod2))
qqline(residuals(mod2), col="red")
shapiro.test(residuals(mod2)) # p-value = 0.02332 (normal)

## Multiple linear regression
mod2=lm(Corruption~GDP+Freedom+Generosity,data=happy.remv)
summary(mod2)


# Q5
mod3=lm(Corruption~Freedom*Region,data=happy.remv)
summary(mod3)


# Q6
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
head(cancer)
unique(cancer$malignant)

## Test multi-collinearity
pairs(cancer[,])
cor(cancer[,], use="na.or.complete")
cor(cancer[, c("radius_mean","texture_mean","smoothness_mean")], use="na.or.complete")
library(fmsb)
VIF(lm(malignant~radius_mean+texture_mean,data=cancer)) # VIF = 2.318363 < 10 (no multi-collinearity)
VIF(lm(malignant~texture_mean+smoothness_mean,data=cancer)) # VIF = 1.445246 < 10 (no multi-collinearity)
VIF(lm(malignant~radius_mean+smoothness_mean,data=cancer)) # VIF = 2.435185 < 10 (no multi-collinearity)

mod4=lm(malignant~radius_mean+texture_mean+smoothness_mean,data=cancer)

## Test residual independency
plot(residuals(mod4))

## Test linear relationship
cancer$malignant=factor(cancer$malignant)
mod5 = glm(malignant~radius_mean+texture_mean+smoothness_mean,data=cancer,family='binomial')
plot(residuals(mod5))
plot(residuals(mod5)~fitted(mod5))
abline(lm(residuals(mod5)~fitted(mod5)),col="red")

## Generalized linear regression
cancer$malignant=factor(cancer$malignant)
mod5 = glm(malignant~radius_mean+texture_mean+smoothness_mean,data=cancer,family='binomial')
summary(mod5)


# Q7
anova(mod5)

## Standardize the scale of independent variables
cancer = cancer %>%
  mutate(radius_mean = (cancer$radius_mean-mean(cancer$radius_mean))/sd(cancer$radius_mean)) %>%
  mutate(texture_mean = (cancer$texture_mean-mean(cancer$texture_mean))/sd(cancer$texture_mean)) %>%
  mutate(smoothness_mean = (cancer$smoothness_mean-mean(cancer$smoothness_mean))/sd(cancer$smoothness_mean))

## Generalized linear regression
cancer$malignant=factor(cancer$malignant)
mod6 = glm(malignant~radius_mean+texture_mean+smoothness_mean,data=cancer,family='binomial')
summary(mod6)
anova(mod6)
