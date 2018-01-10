### NRE 538 Take-home Final
### Charlotte Weinstein
### Apr 23, 2017

#Load data from github:
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

##Question 1:
#Because gender and unruly_child are both categorical, use a chi-sq test
#Do I need to clean this data first?

#construct contingency table
table(flying$gender, flying$unruly_child)
chisq.test(flying$gender, flying$unruly_child)
#p-value indicates that there is a significant difference

##Question 2:
#Because tuition is continuous and institution type is categorical,and there are 
#only 2 levels of the categorical, use t-test.
#Checking assumptions:
#Check that data is normally distributed
#First look at histogram:
hist(college$tuition)
#looks slightly skewed to the right. Use Shapiro-Wilk and qqplot to double-check
shapiro.test(college$tuition)
#p-value is 3.193e-15, which means we reject the null that the data is normal.
#try a qqplot:
qqnorm(college$tuition); qqline(college$tuition, col="Red")
#qqplot looks pretty ugly.
#Try using a log transform:
logtuition = log(college$tuition)
qqnorm(logtuition); qqline(logtuition, col="Red")
#this made it worse
#Try a sqrt transform:
sqrttuition = sqrt(college$tuition)
qqnorm(sqrttuition); qqline(sqrttuition, col="Red")
#This looks slightly better. Check histogram and shapiro-wilk.
hist(sqrttuition)
shapiro.test(sqrttuition)
#Histogram still doesn't look as normal and data doesn't pass the shapiro-wilk test.
#Because we have more than 30 samples, with more than 15 in each group, we can relax
#the normality assumption and proceed anyway.

#check for equal variance:
#First, subset data for Public and Private nonprofit
public = subset(college, type=="Public")
nonprofit = subset(college, type=="Private nonprofit")

var.test(public[,"tuition"], nonprofit[,"tuition"])
#variance is not equal. Can get around this by using the Welch's t-test

#run the two-sample t-test:
t.test(public[,"tuition"], nonprofit[,"tuition"], paired=FALSE)
#p-value indicates significant difference

#make a boxplot to determine which type has greater tuition
boxplot(tuition~type, data=college, xlab="Type", ylab="Tuition")
#the private nonprofit has higher tuition.

###Question 3:

#Use an ANOVA.

#Check for normality of data:
#First check histogram
hist(happy$Hscore)
#histogram looks kind of decent. Use Shapiro-Wilk and qqplot to double-check
shapiro.test(happy$Hscore)
#p-value is 0.012, which means we reject the null that the data is normal.
#try a qqplot:
qqnorm(happy$Hscore); qqline(happy$Hscore, col="Red")
#qqplot looks pretty ugly.
#Try using a log transform:
logHscore = log(happy$Hscore)
qqnorm(logHscore); qqline(logHscore, col="Red")
shapiro.test(logHscore)
#this didn't improve the qqplot, made shapiro test worse
#Try a sqrt transform:
sqrtHscore = sqrt(happy$Hscore)
qqnorm(sqrtHscore); qqline(sqrtHscore, col="Red")
shapiro.test(sqrtHscore)
#This didn't help either

#We can assume normality because there are more than 30 samples, with >15 for each group.
#Proceed with the ANOVA.

#Check for equal variance using the Levene Test:
library(car)
leveneTest(Hscore~Region, data=happy)

#Run the ANOVA test and view results
Region.aov = aov(Hscore~Region, data=happy)
summary(Region.aov)



### Question 4:

#build lm of Corruption ~ Freedom + GDP + Life

#Check normality of dependent variable:
hist(happy$Corruption)
shapiro.test(happy$Corruption)
qqnorm(happy$Corruption); qqline(happy$Corruption, col="Red")
#definitely not normal. Try transforming the data:
hist(log(happy$Corruption))
shapiro.test(log(happy$Corruption))
qqnorm(log(happy$Corruption)); qqline(log(happy$Corruption), col="Red")
#log-transforming the data lets it pass the Shaprio-Wilk test and makes the 
#qqplot look much better.

#create linear model with log-transformed dependent variable
corruptlm = lm(log(Corruption)~Freedom+GDP+Life, data=happy)

#check for correlation between variables
df = data.frame(happy$Freedom, happy$GDP, happy$Life)
cor(df)

#check for independence of errors
plot(residuals(corruptlm))
library(lmtest)
dwtest(corruptlm, alternative=c("two.sided"))

#check for homoscedasticity
plot(residuals(corruptlm)~fitted(corruptlm))
abline(lm(residuals(corruptlm)~fitted(corruptlm)), col="red")

#check for normality of error distribution
qqnorm(residuals(corruptlm))
qqline(residuals(corruptlm), col="red")
shapiro.test(residuals(corruptlm))

#look at results of model
summary(corruptlm)

### Question 5:

#Make lm with Corruption ~ Freedom*Region
#Still use log-transformed data for normality.
#assumptions as described in question 4 still apply

#create and look at lm
lm2 = lm(log(Corruption)~Freedom+Freedom*Region, data=happy)
summary(lm2)


### Question 6:

#data exploration: see which variables are correlated, to avoid multi-collinearity
cancerdf = data.frame(cancer$radius_mean, cancer$texture_mean, cancer$perimeter_mean, cancer$area_mean, cancer$smoothness_mean)
cor(cancerdf)

#use radius_mean, texture_mean, and smoothness_mean

#create a glm using binomial family & logit link function
cancerglm = glm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(cancerglm)


### Bonus:

#Use model checking to assess which GLM has best fit.
cancerglm1 = glm(malignant~radius_mean, data=cancer, family=binomial(link="logit"))
cancerglm2 = glm(malignant~texture_mean, data=cancer, family=binomial(link="logit"))
cancerglm3 = glm(malignant~smoothness_mean, data=cancer, family=binomial(link="logit"))

#Compare using anova, using Chisq becaues this is binomial data (and we don't have RSS)
anova(cancerglm1, cancerglm2, cancerglm3, test="Chisq")

#Compare AIC for the three models:
AIC(cancerglm1, cancerglm2, cancerglm3)
#cancerglm1 has the lowest AIC by a wide margin, which implies that this is the 
#best fit. This means that radius_mean is most important in explaining whether 
#a tumor is malignant.