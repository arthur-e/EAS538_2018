###NRE 538 Final Exam
#Load data
library(RCurl)
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
###

###
###Question 1-flying dataset
#Use chi-square (both variables are categorical)
#Checking assumptions-assume independent observations and no structural 0's.
chisq.test(flying$gender, flying$unruly_child)#p=0.001193
#ANSWER: There is a significant association between gender and whether people think it's rude to bring an unruly child onto a plane. 

###Question 2-college dataset
#Use 2 sample t-test (only 2 types of school)
#checking assumptions
#data are continuous, assume random sampling and independent observations
hist(college$tuition)
shapiro.test(college$tuition)#p<<0.0001-data not normally distributed
qqnorm(college$tuition)
qqline(college$tuition, col = "red")#histogram, shapiro.test, and qqplot show data is not normally distributed, but sample size is large (>500) so we assume normality under CLT.
public1 = subset(college, type=="Public")
private1 = subset(college, type=="Private nonprofit")#subsetting data
var.test(public1$tuition, private1$tuition)#variance test shows unequal variances so we use Welch's 2 sample t-test.
t.test(public1$tuition, private1$tuition)#p<<<0.001 and mean of public school tuition is far lower than private.
#ANSWER: There is a significant difference in tuition by type of institution, and tuition is significantly lower for public schools than private nonprofit schools.

###Question 3-happy dataset
#Use one-way anova
#Checking assumption
#data assumed to be independent, sample sizes do not need to be equal.
hist(happy$Hscore)
shapiro.test(happy$Hscore)#p=0.0125-not normally distributed
qqnorm(happy$Hscore)
qqline(happy$Hscore, col = "red")#histogram, shapiro.test, and qqplot show data is not normally distributed but is close in qqplot, but sample size is large (157) so we assume normality under CLT.
install.packages("lawstat")
library(lawstat)
levene.test(happy$Hscore, happy$Region, location = "mean")#p=0.342, variances are equal.
aov1 = aov(happy$Hscore ~ happy$Region, data = happy)#p<<0.001 shows that there is a difference in Hscore by Region.
summary(aov1)
aov2 = lm(happy$Hscore ~ happy$Region, data = happy)#comparisons of region to Africa.
summary(aov2)
aov3 = lm(happy$Hscore-1 ~ happy$Region, data = happy)#comparisons of region to region mean
summary(aov3)
install.packages("DTK")
library(DTK)
Happy = DTK.test(happy$Hscore, happy$Region)#Tukey-krammer Post-hoc test with unequal sample sizes.
DTK.plot(Happy)
Happy
#ANSWER: There is a significant difference in happiness by region. The comparisons that are significantly different from each other are Europe to Africa, Americas to Asia/Australia, Americas to Africa, and Asia to Africa.

###Question 4-happy dataset
#multiple linear regression
#checking model assumptions
hist(happy$Corruption)#not normally distributed
shapiro.test(happy$Corruption)#p<<0.001, not normally distributed
log_corrupt = log(happy$Corruption)#log transform y
hist(log_corrupt)
qqnorm(log_corrupt)
qqline(log_corrupt, col = "red")
shapiro.test(log_corrupt)#transform results in normality, p=0.0835
pairs(happy[, c("GDP", "Freedom", "Generosity")])#no evidence of multicollinearity.
mod1 = lm(log_corrupt ~ GDP + Freedom + Generosity, data = happy)#run model
library(lmtest)
bptest(mod1)#errors are homoscedatistic, p=0.06878
dwtest(mod1)#errors are independent, p=0.185
summary(mod1)#summary of model, model fit: Adjusted R2=0.1952, explains almost 20% of variance in Corruption (y).
#ANSWER: Out of GDP, Freedom, and Generosity, only Freedom was significantly associated with Corruption and Corruption incresed as Freedom increased (interestingly enough).

###Question 5-happy dataset
#Use ANCOVA lm framework
#We already tested for normality in y-use log transform
mod2 = lm(log_corrupt ~ Freedom * Region, data = happy)#run ANCOVA model
bptest(mod2)#p=0.2501, homoscedaststic errors
dwtest(mod2)#p=0.7187, independence of errors
summary(mod2)#There is a difference of the influence of freedom on Corruption by region.
#Adj R2=0.3053
#ANSWER: The influence of freedom on corruption does vary by region. The only difference is in Europe where corruption increases as freedom increases compared to Africa/Middle East, p=0.000494.

###Question 6-cancer dataset
#Use a binomial logistic regression glm
#checking model assumptions-assume linear relationship, that y falls in binomial distribution (0, 1 data), does not need to assume homoscedastisty.
pairs(cancer[, c("texture_mean", "area_mean", "smoothness_mean", "radius_mean", "perimeter_mean")])#since area is directly related to radius and perimeter, we can only use area, smoothness, and texture as variables to avoid mulitcollinearity.
mod3 = glm(malignant ~ texture_mean + area_mean + smoothness_mean, data = cancer, family = binomial(link="logit"))
summary(mod3)#All independent variables are significant.
mod4 = glm(malignant ~ texture_mean + smoothness_mean, data = cancer, family = binomial(link="logit"))
summary(mod4)
mod5 = glm(malignant ~ smoothness_mean, data = cancer, family = binomial(link="logit"))
summary(mod5)
AIC(mod3, mod4, mod5)#checking main model with other models revealed main model has better fit (far lower AIC).
#ANSWER: mean texture, mean smoothness, and mean area are all significantly associated with wether or not a tumor is malignant.

###Question 7(BONUS)-cancer dataset
#Use question 6 model-which variable explains most variation in y?
install.packages("caret")
library(caret)
varImp(mod3, scale = FALSE)#returns value of importance for each variable.
#ANSWER: area_mean has highest value of importance as returned by the VarImp function (8.9), so we can conclude that this has a greater effect on malignancy. 

