#Q1
flying = read.csv('flying.csv')
tbl = table(flying$gender, flying$unruly_child)
tbl
chisq.test(tbl)
install.packages("RCurl")
library(RCurl)

#Q2
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
qqnorm(college$tuition); qqline(college$tuition, col="Red")
shapiro.test(college$tuition)
private = subset (college, type=="Private nonprofit")
public = subset(college, type=="Public")
var.test(college$tuition~type,data=college)
collegetuition<-t.test(private$tuition, public$tuition, paired=FALSE)
hist(private$tuition, breaks=15, col="light blue", xlim=c(0, 50000), ylim=c(0,300), xlab="tuition", main="")
abline(v=mean(private$tuition), col="blue")
par(new=TRUE)
hist(public$tuition, breaks=15, col="light green", xlim=c(0, 50000), ylim=c(0,300), xlab="", main="")
abline(v=mean(public$tuition), col="dark green")
legend("topright", legend = c("private", "public"),text.col=c("blue", "dark green"))

#Q3
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
shapiro.test(happy$Hscore)
qqnorm(happy$Hscore)
qqline(happy$Hscore, col="red")
library(car)
leveneTest(happy$Hscore~happy$Region,data=happy)
aovout<-aov(Hscore~Region,data=happy)
summary(aovout)
install.packages("DTK")
library(DTK)
kramer<-DTK.test(happy$Hscore, happy$Region)
DTK.plot(kramer)

#Q4
cor(happy$Corruption, happy$Freedom)
cor(happy$Corruption, happy$Generosity)
cor(happy$Corruption, happy$GDP)
hist(happy$Corruption)
shapiro.test(happy$Corruption)
qqnorm(log(happy$Corruption))
qqline(log(happy$Corruption), col="red")
shapiro.test(log(happy$Corruption))


model=lm(log(Corruption)~Freedom+Generosity+GDP,data=happy)

model1=lm(log(Corruption)~Freedom+Generosity+GDP,data=happy)
summary(model1) 
plot(model1)
vif(model1)
shapiro.test(residuals(model1))
qqnorm(model1)
qqline(model1)
hist(residuals(model1))
install.packages(lmtest)
library(lmtest)
dwtest(model1)
bptest(model1)

#Q5
shapiro.test(happy$Freedom)
qqnorm(happy$Freedom)
qqline(happy$Freedom)
leveneTest(happy$(Corruption)~happy$Region,data=happy)
mod = lm(log(Corruption)~ Freedom + Freedom*Region, data=happy)
summary(mod)

#Q6
cancer=read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
pairs(cancer [, C("radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean")])
cor(cancer)
mod3 = glm(malignant~radius_mean + texture_mean + perimeter_mean, data=cancer, family=binomial(link="logit"))
summary(mod3)
dwtest(mod3)
install.packages("clusterSEs")
library(clusterSEs)


BONUS
install.packages("caret")
library(caret)
varImp(mod3)
