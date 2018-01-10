flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

#question 1
flying.new=subset(flying, !is.na(gender) & !is.na(unruly_child))
female=subset(flying.new, gender=="Female")
male=subset(flying.new, gender=="Male")
ct=table(flying.new$gender, flying.new$unruly_child)
Female=c(91, 193, 158)
Male=c(56, 155, 190)
ct=rbind(Female, Male)
colnames(ct)=c("No","Somewhat","Very")
ct
chisq.test(ct)

#question 2
private=subset(college, type=="Private nonprofit")
public=subset(college, type=="Public")
var.test(private$tuition, public$tuition)
shapiro.test(college$tuition)
hist(college$tuition)
qqnorm(college$tuition)
qqline(college$tuition, col="red")
t.test(private$tuition, public$tuition, alternative = "two.sided", var.equal=FALSE)

#question 3
shapiro.test(happy$Hscore)
qqnorm(happy$Hscore)
qqline(happy$Hscore, col="red")
hist(happy$Hscore)
library(car)
leveneTest(happy$Hscore, happy$Region)
happiness=aov(Hscore~Region, data=happy)
summary(happiness)
TukeyHSD(happiness)

#question 4
model=lm(Corruption~GDP + Life + Generosity, data=happy)
library(lmtest)
bptest(model)
dwtest(model)
shapiro.test(happy$Corruption)
hist(happy$Corruption)
log_corruption=log(happy$Corruption)
hist(log_corruption)
sqrt_corruption=sqrt(happy$Corruption)
hist(sqrt_corruption)
shapiro.test(sqrt_corruption)
qqnorm(sqrt_corruption)
qqline(sqrt_corruption, col="red")
mod2=lm(sqrt_corruption~GDP+Life+Generosity, data=happy)
summary(mod2)
vif(mod2)
bptest(mod2)
dwtest(mod2)
shapiro.test(resid)
resid=resid(mod2)
plot(resid)

#question 5
interaction=lm(sqrt_corruption~GDP+GDP*Region, data=happy)
summary(interaction)
resid2=resid(interaction)
plot(resid2)
bptest(interaction)
dwtest(interaction)
shapiro.test(resid2)
hist(resid2)

#question 6
glm=glm(malignant~texture_mean+area_mean+smoothness_mean, data=cancer, family='binomial')
summary(glm)
texture=exp(0.381076)
area=exp(0.016260)
smoothness=exp(146.766602)
texture
area
smoothness
glm2=glm(malignant~texture_mean+area_mean+radius_mean, data=cancer, family='binomial')
summary(glm2)
