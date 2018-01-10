#Take home Final
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
#Question1
head(flying)
tbl=table(flying$gender, flying$unruly_child)
tbl
#Conticiency table shows that the dataset meets assumption
chisq.test(flying$gender, flying$unruly_child)
#P value is less than 0.05, and there is a significant association between gender and whether they think it is rude to bring an unruly child on the plane
#From the Chi-square table we can tell that there are more male choose "very" than female, and less male choose"no" than female. So male tends to think that bringing an unruly child is more rude

#Question 2
#check assumption for two sample t test
#check normality
shapiro.test(college$tuition)
hist(college$tuition)
qqnorm(college$tuition)
qqline(college$tuition)
#failed shapiro test, but since sample size is large, and qq plot and histogram shows it is close to normal. We will run the test as it is
#check equal variance
leveneTest(college$tuition, college$type)
#p value is less than 0.05. We reject the null hypothesis and so the variance are not equal
var(college$tuition)
var(college$type)
#The variance of tuition is more than 3 times larger than variance of type. 
#Run Welch's t test
t.test(tuition~type, var.equal = FALSE, paired=FALSE, data=college)
#Significant differences. Mean of tuition in private nonprofit is higher than mean of tuition in public school. So private nonprofit has a higher tuition.

#Question 3
#check assumption for one-way ANOVA
#check normality-smaple size>30 so I won't worry
shapiro.test(happy$Hscore)
hist(happy$Hscore)
qqnorm(happy$Hscore)
qqline(happy$Hscore)
#check for equal variances-yes variances are equal
library(Rcmdr)
leveneTest(happy$Hscore, happy$Region)
var.test(happy$Hscore, happy$Region)
#Run one-way ANOVA
aov=lm(Hscore~Region, data=happy)
summary(aov)
#p value of less than 0.05 for all regions. Adjusted R^2 is 0.30, which means about 30% variance can be explained by the independent variables. 
#yes, there is significant differences of happeniess score by region

#Question 4
#check for linear relationship
plot(Corruption~Freedom, data=happy)
plot(Corruption~GDP, data=happy)
plot(Corruption~Generosity, data=happy)
plot(Corruption~Family, data=happy)
plot(Corruption~Life, data=happy)
#I choose Freedom, GDP, and Generosity 
#check assumptions
#check depedent variable normality-Very skewed 
shapiro.test(happy$Corruption)
qqnorm(happy$Corruption)
qqline(happy$Corruption)
hist(happy$Corruption)
#log transform data-yes normality met after transform
log=log(happy$Corruption)
#check normality after transform-normality met
hist(log)
qqnorm(log)
qqline(log)
shapiro.test(log)
#Look at correlations + pick non-correlated model
model=(lm(log~GDP+Freedom+Generosity, data=happy))
cor(happy$GDP, happy$Freedom)
cor(happy$Freedom, happy$Generosity)
cor(happy$Generosity, happy$GDP)
vif(model)
#no significant multicollinearity among independent variables
#check for homoscedasticity
plot(model)
install.packages("lmtest")
library(lmtest)
bptest(model)
#we barely pass bptest. The residuals are marginally homoscedastic.
#plot shows the residuals are randomly distributed aroudn fitted line. We prove homoscedasticity
#check for normality of errors-erros are normally distributed
res=residuals(model)
qqnorm(res)
qqline(res)
shapiro.test(res)
#pass shapiro test-normality met
#Run model
summary(model)
#Freedom is significantly associated with a country's corrumption level
#model checking
anova(model)
#anova table confirmed that freedom has significant association with country's corruption (p value and F statistics)

#Question 5
str(happy$Region)
model1=(lm(log~Freedom+Region+Freedom*Region, data=happy))
summary(model1)
library(interplot)
interplot(model1, var1="Freedom", var2="Region")
#Influence of freedome vary by region (significantly influence by Europe)
#In Europe, for each unit increase in freedom, the log of corruption increase by (1.5929+2.9826=4.5755) unit.
#To accurately interpret the result, we need to log transform back the corruption variable

#Question 6
#check for mulcollinearity
cor(cancer$radius_mean, cancer$texture_mean)
cor(cancer$texture_mean, cancer$area_mean)
cor(cancer$smoothness_mean, cancer$area_mean)
cor(cancer$radius_mean, cancer$area_mean)
cor(cancer$smoothness_mean, cancer$texture_mean)
plot(texture_mean~area_mean, data=cancer)
plot(texture_mean~smoothness_mean, data=cancer)
plot(smoothness_mean~area_mean, data=cancer)
#choose three variables that are not autocorrelated 
mod=glm(malignant~texture_mean+smoothness_mean+area_mean, data=cancer, family=binomial(link = "logit"))
summary(mod)
#all three variables have significant association with whether a breast cancer tumor is malignant or not (p value)

##### OSCAR: There should be more than this, right? (-0.5)

#Question 7
#The estimate for smoothness_mean is the biggest, and p value is significanlty less than 0.05
#So campred to teture_mean and area_mean, smoothness_mean is the most important indepedent variable in explaining whether a breast cancer tumor is malignant or not.

##### OSCAR: this can be simply answered by first standardize independent variables and compare the regression coefficients.

