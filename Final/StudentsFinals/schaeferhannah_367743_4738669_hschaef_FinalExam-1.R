## Question 1.
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
str(flying)
newflying = flying[complete.cases(flying),]
str(newflying)
gender = flying$gender
unruly = flying$unruly_child
table = table(gender, unruly)
chisq.test(table)

## Question 2.
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
qqnorm(college$tuition); qqline(college$tuition, col="Red")
shapiro.test(college$tuition)
hist(college$tuition)
logtuition = log(college$tuition)
qqnorm(logtuition)
qqline(logtuition)
shapiro.test(logtuition)
sqrttuition = sqrt(college$tuition)
qqnorm(sqrttuition)
qqline(sqrttuition)
shapiro.test(sqrttuition)
collegesqrt = cbind(college, sqrttuition)
publiccollege = subset(collegesqrt, collegesqrt$type=='Public')
privatecollege = subset(collegesqrt, collegesqrt$type=='Private nonprofit')
var.test(publiccollege[,"sqrttuition"], privatecollege[,"sqrttuition"])
var.test(publiccollege[,"tuition"], privatecollege[,"tuition"])
t.test(privatecollege[,"sqrttuition"], publiccollege[,"sqrttuition"], paired=FALSE, var.equal = FALSE)
mean(privatecollege$sqrttuition)
mean(publiccollege$sqrttuition)


## Question 3.
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
boxplot(Hscore~Region, data=happy, xlab="Region", ylab="Happiness Score)")
hist(happy$Hscore)
happyAmerica = subset(happy, happy$Region=='AmericasCarribean')
happyAsia = subset(happy, happy$Region=='AsiaAustralia')
happyEurope = subset(happy, happy$Region=='Europe')
happyAfrica = subset(happy, happy$Region=='AfricaMideast')
hist(happyAmerica$Hscore)
hist(happyAfrica$Hscore)
hist(happyAsia$Hscore)
hist(happyEurope$Hscore)
anovatest = lm(Hscore~Region, data=happy)
summary(anovatest)

## Question 4.
pairs(happy[, c("Corruption", "Freedom", "Hscore", "Generosity")], use="na.or.complete")
cor(happy[, c("Corruption", "Freedom", "Hscore", "Generosity")], use="na.or.complete")
model3 = lm(Corruption~Freedom+Hscore+Generosity, data=happy)
summary(model3)
anova(model3)
res = residuals(model3)
RSS = sum(res^2)
MSE = RSS/summary(model3)$df[2]
plot(model3)
sqrtcorrupt = sqrt((happy$Corruption))
hist(sqrtcorrupt)
logcorrupt = log(happy$Corruption)
hist(logcorrupt)


## Question 5
interactcorrupt = lm(Corruption~Freedom+Region+Freedom*Region, data=happy)
summary(interactcorrupt)


## Question 6
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
pairs(cancer[, c("radius_mean", "texture_mean", "smoothness_mean")], use="na.or.complete")
cor(cancer[, c("radius_mean", "texture_mean", "smoothness_mean")], use="na.or.complete")
cancermodel = lm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer)
summary(cancermodel)
plot(cancermodel)


