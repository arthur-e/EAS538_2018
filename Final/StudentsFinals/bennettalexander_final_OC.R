flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
## Q1
## the variables are both categorical, test using chi square
## assumption: the observations are independent
## perform chi square
chisq.test(flying$gender, flying$unruly_child)
## the p-value indicates that there is a significant difference between genders
## model the coefficients
lm(formula = unruly_child~gender-1, data=flying)
## conclusion: males tend to find bringing an unruly child to be more rude


college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
## Q2
## check out the independent variable
str(college$type)
## there are two groups, so a t-test is appropriate
## subset the samples
pub = subset(college, type=="Public")
priv = subset(college, type=="Private nonprofit")
## assumption: the dependent variable is continuous and the observations are independent
## assumption: the data are normally distributed
hist(college$tuition)
qqnorm(college$tuition); qqline(college$tuition, col="Red")
## the data look skewed, but we have more than enough observations to apply the CLT
## also, a transformation would result in counterintuitive results for this variable
## assumption: the samples have equal variance
var.test(pub$tuition, priv$tuition)
## the variances are different, but we can use welch's test to relax that assumption
t.test(pub$tuition, priv$tuition)
## there is a significant difference between the types of institutions
## model the coefficients
lm(tuition~type-1, data=college)
## conclusion: private colleges tend to have higher tuitions


happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
## Q3
## check out the data frame
str(happy)
## Hscore is continuous and Region is categorical, so an ANOVA is appropriate
## assumption: the samples are independent
## assumption: the data are normally distributed
hist(happy$Hscore)
qqnorm(happy$Hscore); qqline(happy$Hscore, col="Red")
## the data don't appear to be normal, let's try some transformations
hist(log(happy$Hscore))
hist(sqrt(happy$Hscore))
## these don't look much better, but we have enough observations to apply the CLT
## assumption: the samples have equal variance
bartlett.test(Hscore~Region, data=happy)
## looks good, and we have more than 15 observations in each group anyway
## run the ANOVA
summary(aov(Hscore~Region, data=happy))
## conclusion: there is a significant difference in happiness score by region

## Q4
## assumption: the data are normally distributed
hist(happy$Corruption)
qqnorm(happy$Hscore); qqline(happy$Hscore, col="Red")
## these data don't look normal either, let's try some transformations
hist(log(happy$Corruption))
shapiro.test(log(happy$Corruption))
hist(sqrt(happy$Corruption))
shapiro.test(sqrt(happy$Corruption))
## the log results just pass the shapiro test, so we'll use that
## also, since this variable is already an index, using the transformation won't change our interpretation much anyway
## hmm, what might correlated with corruption...
mod.h = lm(log(Corruption)~Hscore+Freedom+Generosity, data=happy)
summary(mod.h)
## the freedom index is significantly correlated with corruption
## let's check the model
library(lmtest)
dwtest(mod.h, alternative=c("two.sided"))
bptest(mod.h)
shapiro.test(residuals(mod.h))
## the residuals appear to be independent and homoscedastic, but not normal

## Q5
## since the freedom index was correlated with corruption, we'll see how that interacts with region
## first, let's visualize the relationship between corruption and happiness
plot(happy$Freedom,happy$Corruption)
## surprisingly, a higher freedom value seems to be correlated with a higher corruption value!
## perhaps a higher corruption value actually means there's less corruption...?
## let's check out the model using the transformation
mod.c = lm(log(Corruption)~Freedom*Region, data=happy)
summary(mod.c)
## at a p = 0.05 level of significance, the only significant interaction between the variables is in europe
## this means that the value of the corruption variable (whatever it means) in european countries increases as the freedom variable increases
## (i.e., the slope of this fit line is greater than those of the other regions)
## let's check the model
dwtest(mod.c, alternative=c("two.sided"))
bptest(mod.c)
shapiro.test(residuals(mod.c))
## again, the residuals appear to be independent and homoscedastic, but not normal


cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
## Q6
## check out the data frame
str(cancer)
## everything is continuous other than malignancy
## let's check out how the variables relate to each other
pairs(cancer)
## radius, perimeter, and area look like they have the strongest correlations with malignancy
## since the dependent variable is binomial, we'll use a glm
mod.m = glm(malignant~radius_mean+perimeter_mean+area_mean, data=cancer, family=binomial(link="logit"))
summary(mod.m)
## all of these variables are significantly associated with malignancy
## let's check the model
plot(mod.m, which=1)
## the model appears to have some issues, but it's difficult to interpret this given the binomial distribution

##### OSCAR: Using plot() function here might not be helpful as it is a binomial data. 
#####        One possible model checking is to use AIC to compare this model to others (e.g. the null model)

## Q7
## invert the link function
exp(mod.m$coefficients)
## the perimeter and area of the tumor are the most important predictors of malignancy
## for example, the odds ratio for malignancy at the average perimeter is 2.95

##### OSCAR: You'll have to standardize the independent variables to answer this question.