setwd("C:/Users/helen/Documents/winter_2017/538_stats")

flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
library(car)
library(dplyr)
library(reshape2)
library(RCurl)
library(ggplot2)

#QUESTION 1: Is there a sig association between "gender" and "unruly child"?
head(flying)
str(flying)
con.table = table(flying$gender, flying$unruly_child)
rudeness = chisq.test(table(flying$gender, flying$unruly_child))
rudeness

#QUESTION 2: Is there a sig dif in tuition by type of institution?
head(college)
str(college)
boxplot(tuition~type, data=college, xlab="type of institution", ylab="tuition")
public = subset(college, type=="Public")
private = subset(college, type=="Private nonprofit")
#Assumptions
shapiro.test(college$tuition)
qqnorm(college$tuition); qqline(college$tuition, col="Red")
t.test(public$tuition, private$tuition, var.equal = FALSE, paired = FALSE)
leveneTest(tuition~type, data = college) #unequal variance

#QUESTION 3: Does happiness vary significantly by region? --> ANOVA
head(happy)
str(happy)
boxplot(Hscore~Region, data = happy, xlab = "Region", ylab = "Happiness Score")
#Assumptions:
shapiro.test(happy$Hscore)
qqnorm(happy$Hscore); qqline(happy$Hscore, col="Red")
leveneTest(Hscore~Region, data = happy) #equal variance

ANOVA = aov(Hscore~Region, data = happy)
summary (ANOVA)
TukeyHSD(ANOVA)

#QUESTION 4: What factors are significantly associated with corruption?
pairs(happy[,c(5:10)])
cor(happy[,c(5:10)], use="na.or.complete")

#Normality
hist(happy$Corruption)
shapiro.test(happy$Corruption) #not normal
qqnorm(happy$Corruption); qqline(happy$Corruption, col="Red")

log.cor = log(happy$Corruption)
shapiro.test(log.cor) #normal
qqnorm(log.cor); qqline(log.cor, col="Red")
#linearity:
plot(log.cor~GDP, data = happy)
abline(lm(log.cor~GDP, data = happy), col="red")

plot(log.cor~Freedom, data = happy)
abline(lm(log.cor~Freedom, data = happy), col="red")

plot(log.cor~Generosity, data = happy)
abline(lm(log.cor~Generosity, data = happy), col="red")

#Linear model
mod.cor = lm(log.cor ~ GDP + Freedom + Generosity, data = happy)
summary(mod.cor)

#residuals
plot(residuals(mod.cor)~fitted(mod.cor)) #equal variance
abline(lm(residuals(mod.cor)~fitted(mod.cor)), col="red")
library(lmtest)
bptest(mod.cor) #residuals = homoscedastic
#independence of errors
plot(residuals(mod.cor)) #residuals are independent
vif(mod.cor) #residuals are independent
#normality of errors
qqnorm(residuals(mod.cor)) #most of the residuals fit the plot except for a few outliers
qqline(residuals(mod.cor), col = "red")
shapiro.test(residuals(mod.cor)) #non-normal

#QUESTION 5: ANCOVA
library(ggplot2)
library(dplyr)
ggplot(data = happy, mapping=aes(x=Freedom, y=log.cor, color=factor(Region)))+
  geom_point()
ggplot(data=happy, mapping=aes(x=Freedom)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
qqnorm(happy[,"Freedom"]) #not great, but will assume normality from sample size --> CLT
qqline(happy[,"Freedom"], col="red")
shapiro.test(happy[,"Freedom"]) #not normal

cor.mod = lm(log.cor~ Freedom*Region, data=happy)
summary(cor.mod)
ANCOVA = aov(log.cor~ Freedom*Region, data=happy)
summary(ANCOVA)

#interaction:
library(interplot)
interplot(m=cor.mod, var1="Freedom", var2="Region")+
  labs(x="Region", y="Freedom", title="Estimated Coefficient of Freedom on Region")

#QUESTION 6: GLM
head(cancer)
str(cancer)

#correlations
cor(cancer)
pairs(cancer)

#glm
malignant.mod = glm(malignant~radius_mean + texture_mean + smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(malignant.mod)
plot(malignant~radius_mean + texture_mean + smoothness_mean, data=cancer, ylim=c(0,1), ylab="probability of cancer")