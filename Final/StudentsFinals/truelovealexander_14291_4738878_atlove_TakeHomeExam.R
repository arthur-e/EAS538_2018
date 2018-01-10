#FLYING DATASET
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

#1)	Is there a significant association between gender (gender) and whether 
#people think it’s rude to bring an unruly child on the plane (unruly_child)? 
#If yes, which gender tends to think that bringing an unruly child is more rude?

#>> Chi-squared test

tbl = table(flying$gender, flying$unruly_child)
chisq.test(tbl, simulate.p.value = TRUE)

#Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)

#data:  tbl
#X-squared = 13.463, df = NA, p-value = 0.0009995

plot(flying$gender, flying$unruly_child)


#COLLEGE DATASET
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

#2)	Is there a significant difference in tuition (tuition) by type of 
#institution (type)? If yes, which type has a higher tuition?

#>> Unpaired t-test
#>> CHECK: normality of dependent variable (qqplot), equal variance

qqnorm(college$tuition); qqline(college$tuition, col="Red")
hist(college$tuition)

mean(college$tuition)

Public.U = subset(college, type=="Public")
Private.U = subset(college, type=="Private nonprofit")
t.test(Public.U[,"tuition"], Private.U[,"tuition"], paired=FALSE)

Welch Two Sample t-test

#data:  Public.U[, "tuition"] and Private.U[, "tuition"]
#t = -22.79, df = 1397.9, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
  -10258.878  -8632.749
#sample estimates:
#  mean of x   mean of y 
#   18855.88     28301.69 


#HAPPY DATASET
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

#3)	Is there a significant difference in happiness (Hscore) by region (Region)?

#>> ANOVA
#>> CHECK: normality of dependent variable (qqplot), equal variance

qqnorm(happy$Hscore); qqline(happy$Hscore, col="Red")
hist(happy$Hscore)

happy.aov1 = lm(Hscore~Region-1, data=happy)
summary(happy.aov1)

#Call:
#  lm(formula = Hscore ~ Region - 1, data = happy)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.1138 -0.6861 -0.1210  0.5650  2.8019 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  RegionAfricaMideast       4.6021     0.1251   36.78   <2e-16 ***
#  RegionAmericasCarribean   6.1418     0.1906   32.22   <2e-16 ***
#  RegionAsiaAustralia       5.3494     0.1945   27.50   <2e-16 ***
#  RegionEurope              5.9230     0.1348   43.95   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.953 on 153 degrees of freedom
#Multiple R-squared:  0.9708,	Adjusted R-squared:   0.97 
#F-statistic:  1270 on 4 and 153 DF,  p-value: < 2.2e-16

happy.aov2 = aov(Hscore~Region-1, data=happy)
TukeyHSD(happy.aov2)

#$Region
#                                   diff         lwr         upr     p adj
#AmericasCarribean-AfricaMideast  1.5397193  0.94745533  2.13198329 0.0000000
#AsiaAustralia-AfricaMideast      0.7472543  0.14642988  1.34807874 0.0081572
#Europe-AfricaMideast             1.3208593  0.84313961  1.79857901 0.0000000
#AsiaAustralia-AmericasCarribean -0.7924650 -1.49989372 -0.08503628 0.0214193
#Europe-AmericasCarribean        -0.2188600 -0.82522748  0.38750748 0.7847082
#Europe-AsiaAustralia             0.5736050 -0.04112656  1.18833656 0.0768934


#4)	What factors are significantly associated with a country’s corruption 
#levels (Corruption)? Choose three continuous independent variables to include 
#in your model.

#>> multilinear regression
#>> CHECK: normality, homoskedasicity, independence 

qqnorm(happy$Corruption); qqline(happy$Corruption, col="Red")
hist(happy$Corruption)
#NOT NORMAL!

happy$sqrtCorruption = sqrt(happy$Corruption)
hist(happy$sqrtCorruption)
qqnorm(happy$sqrtCorruption); qqline(happy$sqrtCorruption, col="Red")
shapiro.test(pokemon$sqrt_HP)

Corrupt1 = lm(sqrtCorruption~Generosity, data=happy)
summary(Corrupt1)
#p-value: 0.0002481

Corrupt2 = lm(sqrtCorruption~Freedom, data=happy)
summary(Corrupt3)
#p-value: 2.1e-11

Corrupt4 = lm(sqrtCorruption~Life, data=happy)
summary(Corrupt4)
#p-value: 0.01052

Corrupt5 = lm(sqrtCorruption~GDP, data=happy)
summary(Corrupt5)
#p-value: 0.001927

Corrupt6 = lm(sqrtCorruption~Family, data=happy)
summary(Corrupt6)
#p-value: 0.02925

Corrupt7 = lm(sqrtCorruption~Hrank, data=happy)
summary(Corrupt7)
#p-value: 3.876e-06

Corrupt8 = lm(sqrtCorruption~Hscore, data=happy)
summary(Corrupt8)
#p-value: 1.551e-06

cor(happy[c("Generosity","GDP","Family","Life","Freedom","Hrank","Hscore")], use="na.or.complete")

CorruptM1 = lm(sqrtCorruption~Freedom+Generosity+GDP, data=happy)
summary(CorruptM1)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.15423    0.03136   4.918 2.23e-06 ***
#  Freedom      0.37025    0.07511   4.929 2.12e-06 ***
#  Generosity   0.14761    0.07618   1.938   0.0545 .  
#GDP          0.03414    0.02470   1.382   0.1689    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.1167 on 153 degrees of freedom
#Multiple R-squared:  0.2642,	Adjusted R-squared:  0.2497 
#F-statistic: 18.31 on 3 and 153 DF,  p-value: 3.34e-10


#5)	Choose one of the continuous independent variables that was significant 
#in the model for Question 4 and interact it with region (Region) to predict 
#corruption (Corruption). This model should only include one continuous 
#independent variable and its interaction with region. Does the influence of 
#your continuous variable on corruption vary by region? If yes, how do you 
#interpret the interaction?

library(interplot)

CorruptM2 = lm(sqrtCorruption~Freedom*Region, data=happy)
summary(CorruptM2)

plot(happy$Region, happy$Freedom)
interplot(m=CorruptM2, var1="Region", var2="Freedom")


#FLYING DATASET
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

#6)	Which factors are significantly associated with whether a breast cancer 
#tumor is malignant or not? Choose three continuous independent variables to 
#include in your model.

#>> multilinear regression
#>> CHECK: normality, homoskedasicity, independence 

library(faraway)

qqnorm(cancer$malignant); qqline(cancer$malignant, col="Red")
hist(cancer$malignant)
#NOT NORMA, BINOMIAL

cor(cancer[c("radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean")], use="na.or.complete")

CancerGLM1 = glm(malignant~area_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(CancerGLM1)

#7)	BONUS/EXTRA CREDIT: Which independent variables are the most important 
#in explaining whether a breast cancer tumor is malignant or not? Use the same 
#3 continuous independent variables you chose for question 6. cancer.

CancerGLM2 = glm(malignant~area_mean, data=cancer, family=binomial(link="logit"))
summary(CancerGLM2)

CancerGLM3 = glm(malignant~texture_mean, data=cancer, family=binomial(link="logit"))
summary(CancerGLM3)

CancerGLM4 = glm(malignant~smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(CancerGLM4)
