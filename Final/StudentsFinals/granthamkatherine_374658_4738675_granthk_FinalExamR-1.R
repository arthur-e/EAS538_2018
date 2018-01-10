setwd("~/Documents/Nat Resource Stats R")
getwd()

#Question 1-- Chi-Squared
flying=read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

chisq.test()
fly=subset(flying, !is.na(unruly_child), !is.na(gender))
FlyCHI=table(fly$gender, fly$unruly_child)
FlyCHI
chisq.test(fly$gender, fly$unruly_child)


#Question 2
college=read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
head(college)
boxplot(tuition~type, data = college, main="Type of Institution Tuition", ylab="Tuition", xlab="Type of Institution")

College.Public=subset(college, type=="Public")
College.Priv=subset(college, type=="Private nonprofit")

qqnorm(college$tuition); qqline(college$tuition, col="Red")
shapiro.test(College.Priv$tuition)
shapiro.test(College.Public$tuition)
var.test(College.Priv$tuition, College.Public$tuition)

t.test(College.Priv$tuition, College.Public$tuition, var.equal = FALSE, paired = FALSE)

#Question 3
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
head(happy)
qqnorm(happy$Hscore); qqline(happy$Hscore, col="Red")
shapiro.test(happy$Hscore)
install.packages("car")
library(car)
leveneTest(Hscore~Region, data=happy)
Happymod=aov(Hscore~Region, data=happy)
summary(Happymod)
kruskal.test(Hscore~Region, data = happy)

#Question 4
cor(happy)
pairs(happy)
plot(Corruption~Life, data = happy)
plot(Corruption~GDP, data = happy)
plot(Corruption~Freedom, data = happy)
plot(Corruption~Family, data = happy)

CorrMod=lm(Corruption~Family+GDP+Life, data = happy)
summary(CorrMod)

happy$transcor <- sqrt(happy$Corruption)

install.packages("lmtest")
library(lmtest)


mod1=lm(Corruption~Generosity+Family+Hscore, data = happy)
summary(mod1)
dwtest(mod1)
bptest(mod1)
shapiro.test(residuals(mod1))

mod2=lm(transcor~Generosity+Family+Hscore, data=happy)
vif(mod2)
summary(mod2)
dwtest(mod2)
bptest(mod2)
shapiro.test(residuals(mod2))

#Question 5
aov=aov(Corruption~Generosity*Region, data = happy)
summary(aov)
anova(aov)
dwtest(aov)
bptest(aov)
shapiro.test(residuals(aov))
plot(residuals(aov))

aov2=aov(transcor~Family*Region, data=happy)
summary(aov2)
dwtest(aov2)
bptest(aov2)
shapiro.test(residuals(aov2))

#Question 6
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
cor(cancer)
pairs(cancer)

gg=glm(malignant~texture_mean+area_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(gg)

gg2=glm(malignant~texture_mean+radius_mean+area_mean, data=cancer)
summary(gg2)
#Question 7
anova(gg)
