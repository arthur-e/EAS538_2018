library(dplyr)
library(car)
library(lmtest)

#### FLYING DATA
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
head(flying)

#QUESTION 1
#dependent and independent = categorical; chi-square

flytable = table(flying$gender, flying$unruly_child)
head(flytable)

chisq.test(flytable)
#p-value = 0.001193, so there is a significant difference between genders

female = subset(flying, gender=="Female")
male = subset(flying, gender=="Male")

MaleRude = (length(which(male$unruly_child=="Very")))/ ((length(which(male$unruly_child=="Very")))+(length(which(male$unruly_child=="Somewhat")))+(length(which(male$unruly_child=="No"))))
head(MaleRude)  # 0.4738155

FemaleRude = (length(which(female$unruly_child=="Very")))/ ((length(which(female$unruly_child=="Very")))+(length(which(female$unruly_child=="Somewhat")))+(length(which(female$unruly_child=="No"))))
head(FemaleRude) # 0.3574661

#### COLLEGE DATA
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
head(college)

## QUESTION 2
#independent = categorical, dependent = continuous; T-Test

public = subset(college, type=="Public")
private = subset(college, type=="Private nonprofit")

#visual
boxplot(public$tuition, private$tuition, names=c("Public", "Private Non-Profit"), ylab=("Tuition in dollars"))

#normality
shapiro.test(college[,"tuition"]) #p-value = 3.193e-15 (less than .05), so the distribution is not normal. However, the sample size is >30 (1407 observations), so normality can be assumed with the large sample size.

#equal variances
var.test(public$tuition, private$tuition) #p-value < 2.2e-16 (less than .05), (smaller than .05) so equal variances cannot be assumed 

#run test
t.test (public[,"tuition"], private["tuition"], alternative= c("two.sided"), paired=FALSE, var.equal=FALSE)
#p-value < 2.2e-16, so we can reject the null hypothesis and say that there is a significant difference between tuition at public and private non-profit universities.


####HAPPY DATA
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
head(happy)

## QUESTION 3
#independent = categorical, dependent = continuous; one-way ANOVA 

#visual
boxplot(Hscore~Region, data=happy, xlab="Region", ylab="Happiness Score")

#normality
shapiro.test(happy[,"Hscore"]) #p-value = 0.01248 (less than .05), so the distribution is not normal. However, the sample size is >30 (157 observations), so normality can be assumed with the large sample size.
qqPlot(happy$Hscore)

Africa = subset(happy, Region=="AfricaMideast") #58 observations
AsiaAus = subset(happy, Region=="AsiaAustralia") #24 observations
Europe = subset(happy, Region=="Europe") #50 observations
Americas = subset(happy, Region=="AmericasCarribean") #25 observations

#equal variances
leveneTest(Hscore~Region, data=happy) #p-value = 0.5179 (greater than .05), so equal variances can be assumed 

#run test 
happy.aov = aov(Hscore~Region, data=happy)
summary(happy.aov)
#p-value = 1.28e-12 (less than .05), therefore there is a significant difference between Region and happiness score.

## QUESTION 4
#multiple independent = continuous, dependent = continuous; Multiple Linear Regression

cor(happy[,c(4:10)], use="na.or.complete")
#I want to select three independent that aren't highly correlated with each other

cor(happy$GDP, happy$Freedom) #0.3622828
cor(happy$GDP, happy$Generosity) #-0.02553066
cor(happy$Generosity, happy$Freedom) #0.3617513

vif.GDP = 1/(1 - summary(lm(GDP~Freedom+Generosity, data=happy))$r.squared)
vif.GDP #1.189712

vif.Free = 1/(1 - summary(lm(Freedom~GDP+Generosity, data=happy))$r.squared)
vif.Free #1.367952

vif.Gen = 1/(1 - summary(lm(Generosity~Freedom+GDP, data=happy))$r.squared)
vif.Gen #1.189185

#normality of dependent variable
shapiro.test(happy[,"Corruption"]) #p-value = 5.128e-11 (less than .05), so the distribution is not normal.
qqPlot(happy$Corruption)
hist(happy[,"Corruption"], breaks=15, col="light green", xlim=c(0, .6), ylim=c(0,60), xlab="", main="")

#transform corruption

happy = happy %>%
  mutate(LogCorruption = log(Corruption))

qqPlot(happy$LogCorruption)
hist(happy[,"LogCorruption"], breaks=15, col="light green")
shapiro.test(happy[,"LogCorruption"]) #p-value = 0.08535 (greater than .05) so the new distribution is normal!

#visual / linear relationship
plot(happy$GDP, happy$LogCorruption)
plot(happy$Freedom, happy$LogCorruption)
plot(happy$Generosity, happy$LogCorruption)

mod=lm(LogCorruption~GDP + Freedom + Generosity, data=happy)
summary(mod)

#Residual Independency 
plot(residuals(mod)) #does not appear to be a trend
dwtest(mod) #p-value = 0.185 (greater than .05), which means that there is not an autocorrelation and residuals are independent

#Residual Homoscedasticity 
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)), col="red") 
bptest(mod) #p-value = 0.06878 so not heteroscedastic

#Residual normality 
qqnorm(residuals(mod))
qqline(residuals(mod), col="red")

shapiro.test(residuals(mod)) #p-value = 0.0006382 not normal

#run test 
summary(mod)

##QUESTION 5
#independent - continuous + categorical, dependent - continuous; ANCOVA

mod1=lm(LogCorruption~Freedom*Region, data=happy)
summary(mod1)

#Residual Independency 
plot(residuals(mod1)) #does not appear to be a trend
dwtest(mod1) #p-value = 0.7187 (greater than .05), which means that there is not an autocorrelation and residuals are independent

#Residual Homoscedasticity 
plot(residuals(mod1)~fitted(mod1))
abline(lm(residuals(mod1)~fitted(mod1)), col="red") 
bptest(mod1) #p-value = 0.2501 so residuals not homoscedastic

#Residual normality 
qqnorm(residuals(mod1))
qqline(residuals(mod1), col="red")

shapiro.test(residuals(mod1)) #p-value = 0.01481 not normal

#run ANCOVA
mod1=lm(LogCorruption~Freedom*Region, data=happy)
summary(mod1)

#### CANCER DATA
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

##Question 6
#independent - 3 continuous, Dependent - binomial; GLM

cor(cancer[,c(1:6)], use="na.or.complete")
#I want to select three independent that aren't highly correlated with each other

cor(cancer$texture_mean, cancer$smoothness_mean) #-0.02338852
cor(cancer$texture_mean, cancer$area_mean) #0.3210857
cor(cancer$smoothness_mean, cancer$area_mean) # 0.1770284 

vif.Text = 1/(1 - summary(lm(texture_mean~smoothness_mean+area_mean, data=cancer))$r.squared)
vif.Text #1.123269

vif.Smooth =  1/(1 - summary(lm(smoothness_mean~texture_mean+area_mean, data=cancer))$r.squared)
vif.Smooth #1.040059

vif.Area = 1/(1 - summary(lm(area_mean~smoothness_mean+texture_mean, data=cancer))$r.squared)
vif.Area #1.158975

#model
mod2 = glm(malignant~texture_mean+smoothness_mean+area_mean, data=cancer, family=binomial(link="logit"))
summary(mod2)

#model comparison

mod0 = glm(malignant~1, data=cancer, family=binomial(link=logit))
summary(mod0)

AIC(mod0, mod2)
