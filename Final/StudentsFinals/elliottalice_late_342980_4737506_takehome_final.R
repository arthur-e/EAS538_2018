#NRE 538 Take Home Final
#Author: Alice Elliott

flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

#Question 1
tbl = table(flying$gender,flying$unruly_child)
chisq.test(tbl)$expected
chisq.test(tbl)

#Question 2
shapiro.test(college$tuition)
qqnorm(college$tuition)
shapiro.test(sqrt(college$tuition))
public = subset(college, type=="Public")
private = subset(college, type=="Private nonprofit")
t.test(public$tuition,private$tuition,paired=FALSE,var.equal=FALSE)

#Question 3
library(car)
shapiro.test(happy$Hscore)
qqnorm(happy$Hscore)
leveneTest(happy$Hscore,happy$Region) #test for homogeneity of variance
anova = lm(Hscore~Region, data=happy)
summary(anova)

#Question 4
plot(happy$Generosity,happy$Corruption)
abline(lm(happy$Corruption~happy$Generosity))
plot(happy$Hscore,happy$Corruption)
abline(lm(happy$Corruption~happy$Hscore))
plot(happy$GDP,happy$Corruption)
abline(lm(happy$Corruption~happy$GDP))

resid_gen = resid(lm(happy$Corruption~happy$Generosity))
plot(resid_gen)
qqnorm(resid_gen)

resid_hscore = resid(lm(happy$Corruption~happy$Hscore))
plot(resid_hscore)
qqnorm(resid_hscore)

resid_gdp = resid(lm(happy$Corruption~happy$GDP))
plot(resid_gdp)
qqnorm(resid_gdp)

#check for multicollinearity
library(fmsb)
VIF(lm(Corruption~Generosity+Hscore+GDP,data=happy))

mod = lm(happy$Corruption~happy$Generosity+happy$Hscore+happy$GDP)
summary(mod)

#Question 5
mod2 = lm(Corruption~Generosity*Region, data=happy)
summary(mod2)

#Question 6
VIF(lm(malignant~radius_mean+texture_mean+smoothness_mean,data=cancer))
cancer$malignant = factor(cancer$malignant) #convert to categorical data
mod3 = glm(malignant~radius_mean+texture_mean+smoothness_mean,data=cancer,family='binomial') #since the default link is logit, i didn't include it
summary(mod3)

#Bonus Question
mod_radius = glm(malignant~radius_mean, data=cancer, family='binomial')
mod_texture = glm(malignant~texture_mean, data=cancer, family='binomial')
mod_smooth = glm(malignant~smoothness_mean, data=cancer, family='binomial')

AIC(mod_radius,mod_texture,mod_smooth)
