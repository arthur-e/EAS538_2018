flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

####1 Chi-squared test
flying.clean=na.omit(flying)
flying.1=subset(flying.clean, select =c("gender", "unruly_child"))
flying.gen=flying.clean$gender
flying.child=flying.clean$unruly_child
chisq.test(flying.gen,flying.child) #pvalue =.001964
table.1=table(flying.gen,flying.child)
table.1 #males more often say "very rude"

###2 T-test
str(college) #two levels for type
hist(college$tuition) #looks normal and >30 observations
qqnorm(college$tuition)
qqline(college$tuition, col="red")
shapiro.test(college$tuition)
pub.u = subset(college, type=="Public")
pub.tuition=pub.u$tuition
priv.u = subset(college, type=="Private nonprofit")
priv.tuition=priv.u$tuition
qqnorm(priv.tuition)
qqnorm(pub.tuition)
var.test(priv.tuition,pub.tuition) #pvalue~0, variances not equal
hist(pub.tuition, breaks=10, col="light blue", xlim=c(0, 50000), ylim=c(0,300), xlab="Tuition", main="")
abline(v=mean(pub.tuition), col="blue")
par(new=TRUE)
hist(priv.tuition, breaks=15, col="light green", xlim=c(0, 50000), ylim=c(0,300), xlab="", main="")
abline(v=mean(priv.tuition), col="dark green")
legend("topright", legend = c("Public", "Private"),text.col=c("blue", "dark green"))
t.test(priv.tuition,pub.tuition, paired=FALSE, var.equal = FALSE) #welch t-test
#pvalue~0, therefore there is a significant difference between the tuitions of public and private universities
#means of private = $28301.69, public = $18855.88

###3 ANOVA
str(happy) #region has 4 levels
hist(happy$Hscore)
qqnorm(happy$Hscore)
shapiro.test(happy$Hscore)
library(car)
leveneTest(Hscore~Region, data=happy)
boxplot(Hscore~Region, data=happy, ylim=c(0,10),xlab="Region", ylab="Hscore")
anova.happy=aov(Hscore~Region, data=happy) 
summary(anova.happy) #region is significant
TukeyHSD(anova.happy) #Americas-Caribbean highest

###4 Linear regression
library(magrittr)
library(dplyr)
hist(happy$Corruption)
log.happy = mutate(happy, newCorrupt=log(Corruption))
hist(log.happy$newCorrupt)
qqnorm(log.happy$newCorrupt)
qqline(log.happy$newCorrupt, col="red") 
shapiro.test(log.happy$newCorrupt)
pairs(newCorrupt~Freedom+GDP+Generosity, data=log.happy)
plot(newCorrupt~Freedom, data=log.happy)
cor(log.happy[, c("Freedom", "GDP", "Generosity")]) #nothing >.5
corrupt.mod=lm(newCorrupt~Freedom+GDP+Generosity, data=log.happy)
vif(corrupt.mod) #no variable greater than 1.5
summary(corrupt.mod)
#independence
library(lmtest)
dwtest(corrupt.mod, alternative=c("two.sided")) #passes
#homoscedasticity
plot(residuals(corrupt.mod)~fitted(corrupt.mod))
abline(lm(residuals(corrupt.mod)~fitted(corrupt.mod)), col="red")
bptest(corrupt.mod) #passes
#normality
qqnorm(residuals(corrupt.mod))
qqline(residuals(corrupt.mod), col="red") 
plot(corrupt.mod)

###5 Linear regression with interaction
corrupt.mod2=lm(newCorrupt~Freedom*Region, data=log.happy)
summary(corrupt.mod2)

###6 GLM with binomial distribution
cancer1=subset(cancer, select=c(malignant,radius_mean,texture_mean,smoothness_mean))
cor(cancer1) #radius, texture, smoothness not correlated
cancer.mod=glm(malignant~radius_mean+texture_mean+smoothness_mean, family=binomial(link="logit"), data=cancer1)
vif(cancer.mod)
summary(cancer.mod)
plot(cancer.mod)
dwtest(cancer.mod, alternative =c("two.sided")) #there is autocorrelation
qqnorm(residuals(cancer.mod)) #not normal, p-values may be inaccurate
qqline(residuals(cancer.mod), col="red") 
bptest(corrupt.mod) #passes homoscedasticity

###7 GLM R2 comparisons
radius.s = (cancer$radius_mean - mean(cancer$radius_mean)) / sd(cancer$radius_mean)
smoothness.s = (cancer$smoothness_mean - mean(cancer$smoothness_mean)) / sd(cancer$smoothness_mean)
texture.s = (cancer$texture_mean - mean(cancer$texture_mean)) / sd(cancer$texture_mean)
cancer2=mutate(cancer, radius_mean=radius.s, smoothness_mean=smoothness.s, texture_mean=texture.s)
cancer.mod2=glm(malignant~radius_mean+texture_mean+smoothness_mean, family=binomial(link="logit"), data=cancer2)
mod.radius=glm(malignant~radius_mean, family=binomial(link="logit"), data=cancer2)
mod.texture=glm(malignant~texture_mean, family=binomial(link="logit"), data=cancer2)
mod.smooth=glm(malignant~smoothness_mean, family=binomial(link="logit"), data=cancer2)
anova(mod.radius, mod.texture, mod.smooth, test="Chi")
AIC(mod.radius, mod.texture, mod.smooth) #radius is best with lowest AIC score

