library(reshape2)
library(plyr)
library(magrittr)

flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

##1
table(flying$gender,flying$unruly_child)
chisq.test(flying$gender,flying$unruly_child)

##2
qqnorm(college$tuition)
qqline(college$tuition)
shapiro.test(college$tuition)
hist(college$tuition)

transform.s=sqrt(college$tuition)
qqnorm(transform.s)
qqline(transform.s)
shapiro.test(transform.s)

Public=subset(college, type=="Public")
Private=subset(college, type=="Private nonprofit")
pub.tuition=sqrt(Public$tuition)
pri.tuition=sqrt(Private$tuition)
var.test(pub.tuition,pri.tuition)
t.test(pub.tuition, pri.tuition, var=FALSE)
t.test(pub.tuition, pri.tuition, var=FALSE, alternative="less")

##3
hist(happy$Hscore)
qqnorm(happy$Hscore)
qqline(happy$Hscore)
shapiro.test(happy$Hscore)

transform.hs=sqrt(happy$Hscore)
shapiro.test(transform.hs)

transform.hl=log(happy$Hscore)
shapiro.test(transform.hl)

library(car)
leveneTest(Hscore~Region, data=happy)
mod.reg=aov(Hscore~Region, data=happy)
summary(mod.reg)


##4
hist(happy$Corruption)
shapiro.test(happy$Corruption)

trans.cl=log(happy$Corruption)
qqnorm(trans.cl)
qqline(trans.cl)
shapiro.test(trans.cl)

happy = happy %>%
  mutate(transform = (sqrt(Corruption)))

pairs(happy)

cor(happy[,c("GDP","Freedom","Generosity")], use="na.or.complete")

c.mod=lm(transform~GDP+Freedom+Generosity, data=happy)
vif(c.mod)
summary(c.mod)

plot(residuals(c.mod))
library(lmtest)
dwtest(c.mod, alternative=c("two.sided"))

qqnorm(residuals(c.mod))
qqline(residuals(c.mod), col="red")
shapiro.test(residuals(c.mod))

plot(residuals(c.mod)~fitted(c.mod))
abline(lm(residuals(c.mod)~fitted(c.mod)), col="red")
library(lmtest)
bptest(c.mod)

##5
mod.cr=lm(transform~Freedom+Freedom*Region, data=happy)
summary(mod.cr)
anova(mod.cr)

##6
cor(cancer)

mod.glm=glm(malignant~radius_mean+texture_mean+smoothness_mean, 
            data=cancer, family=binomial(link="logit"))
vif(mod.glm)
summary(mod.glm)

plot(mod.glm, which=1)
plot(residuals(mod.glm)~ radius_mean+smoothness_mean+texture_mean, data=cancer)
abline(h=0, col="red")

mod.null = glm(malignant~1, data=cancer, family=binomial(link=logit))
summary(mod.null)
anova(mod.null, mod.glm, test="Chi")
AIC(mod.null, mod.glm)

##7
mod.glm=glm(malignant~radius_mean+texture_mean+smoothness_mean, 
            data=cancer, family=binomial(link="logit"))
mod.rad=glm(malignant~radius_mean, 
            data=cancer, family=binomial(link="logit"))
mod.tex=glm(malignant~texture_mean, 
            data=cancer, family=binomial(link="logit"))
mod.smo=glm(malignant~smoothness_mean, 
            data=cancer, family=binomial(link="logit"))

anova(mod.null, mod.tex, mod.smo, mod.rad, test="Chi")

