flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")


## 1) Is there a significant association between gender (gender) and whether people think it's rude to bring an unruly child 
## on the plane (unruly_child)? If yes, which gender tends to think that bringing an unruly child is more rude?
## [flying]

## Nominal dependent variable (gender) & ordinal independent variable (opinion on unruly children)
## Test with chi-square test of independence.

# Clean data

flying = flying[!(is.na(flying$unruly_child)), ]

# Create contingency table

install.packages('MASS')
library(MASS)

conT = table(flying$gender, flying$unruly_child)
conT

# test

chisq.test(conT)

## 2) Is there a significant difference in tuition (tuition) by type of institution
## (type)? If yes, which type has a higher tuition? [college]

college.private = subset(college, type == "Private nonprofit")
college.public = subset(college, type == "Public")

## Check normality, var etc. of tuition distribution

hist(college$tuition, breaks=100)
shapiro.test(college$tuition)

var.test(college.private$tuition, college.public$tuition)

## perform test

t.test(college.private$tuition, college.public$tuition)

## see which is higher with a box plot

boxplot(tuition~type, data = college)


## 3) Is there a significant difference in happiness (Hscore) by region (Region)? [happy]

# Checking assumptions
hist(happy$Hscore)
shapiro.test(happy$Hscore)

bartlett.test(Hscore~Region, data=happy)

library(Rcmdr)
levene.test(Hscore~Region, data=happy)

# run ANOVA

happy.aov = aov(Hscore~Region, data=happy)
summary(happy.aov)

# run post-hoc to assess magnitude of difference

TukeyHSD(happy.aov)


## 4) What factors are significantly associated with a country's corruption levels (Corruption)? Choose three continuous 
## independent variables to include in your model. [happy]

# Examine dependent variable distribution and transformation 

hist(happy$Corruption, breaks = 30)
hist(log(happy$Corruption), breaks = 30)

shapiro.test(happy$Corruption)
shapiro.test(log(happy$Corruption))

# Add transformed corruption score

library(reshape2)
library(plyr)
library(magrittr)

happy = happy %>%
  mutate(logCorruption = log(Corruption))
head(happy)


# check collinearity

require(lattice)
require(ggplot2)

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y, na.action = na.exclude)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

?cor.test

pairs(happy[, c(3:11)], upper.panel = panel.cor)

# run model

happy.mod1 = lm(logCorruption~Freedom+Hscore+Generosity, data=happy)
summary(happy.mod1)

# check model

vif(happy.mod1)

library(lmtest)

dwtest(happy.mod1)

bptest(happy.mod1)

shapiro.test(residuals(happy.mod1))

plot(happy.mod1)

## 5) Choose one of the continuous independent variables that was significant in the model for Question 4 and interact it with 
## region (Region) to predict corruption (Corruption). This model should only include one continuous independent variable and 
## its interaction with region. Does the influence of your continuous variable on corruption vary by region? If yes, how do you
## interpret the interaction? [happy]

#compare to africa
happy.mod2 = lm(logCorruption~Freedom*Region, data=happy)
summary(happy.mod2)

#compare to mean
happy.mod3 = lm(logCorruption~Freedom*Region-1, data=happy)
summary(happy.mod3)

#compare to Americas
happy$Region = factor(happy$Region, levels=(c("AmericasCarribean", "AfricaMideast", "AsiaAustralia", "Europe")))
happy.mod4 = lm(logCorruption~Freedom*Region, data=happy)
summary(happy.mod4)

#compare to AsiaAustralia
happy$Region = factor(happy$Region, levels=(c("AsiaAustralia", "AmericasCarribean", "AfricaMideast", "Europe")))
happy.mod5 = lm(logCorruption~Freedom*Region, data=happy)
summary(happy.mod5)

#compare to Europe
happy$Region = factor(happy$Region, levels=(c("Europe", "AsiaAustralia", "AmericasCarribean", "AfricaMideast")))
happy.mod6 = lm(logCorruption~Freedom*Region, data=happy)
summary(happy.mod6)


## 6) Which factors are significantly associated with whether a breast cancer tumor is malignant or not? Choose three continuous 
## independent variables to include in your model. [cancer]

# analyze data
pairs(cancer, upper.panel = panel.cor)

# run model
cancer.mod1 = glm(malignant~texture_mean+perimeter_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(cancer.mod1)

# probability of malignance when factors = 0
exp(-40.40324)/(1+exp(-40.40324))

# model check by comparing with null model
cancer.mod0 = glm(malignant~1, data=cancer, family=binomial(link=logit))
summary(cancer.mod0)

anova(cancer.mod0, cancer.mod1, test="Chi")

AIC(cancer.mod0, cancer.mod1)


## 7) BONUS/EXTRA CREDIT: Which independent variables are the most
## important in explaining whether a breast cancer tumor is malignant or not? Use the same 3 continuous independent variables 
## you chose for question 6. [cancer]

#use absolute t-statistic to check
install.packages("caret")
library(caret)

?varImp

varImp(cancer.mod1)

#use standardized beta coefficients to check
install.packages("QuantPsyc")
library(QuantPsyc)

?lm.beta

lm.beta(cancer.mod1)
