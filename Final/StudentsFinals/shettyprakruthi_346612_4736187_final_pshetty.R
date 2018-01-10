library(reshape2)
library(plyr)
library(magrittr)
library(car)
library(lmtest)
library(corrplot)



flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

###--------------------------------------Question 1--------------------------------------###
flying = na.omit(flying)

chisq = chisq.test(flying$unruly_child,flying$gender)
#p:0.001964, null hypothesis is rejected and there exists a dependency on gender as to whether an individual believes its

round(chisq$residuals, 3)
#Since the residual for Males column is high for 'Very', we can say that Males tend to think its more rude than women to have unruly child.

contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)



###--------------------------------------Question 2--------------------------------------###
college = na.omit(college)
count(college,'type')

boxplot(tuition~type,data=college)
#Clearly there seems to be a difference in medians of tuitions for private nonprofit type of institute versus public institutes.

#normality check
hist(college$tuition)
shapiro.test(college$tuition)
#p-value: 3.193e-15, we reject the null hypothesis that data is normal.
qqnorm(college$tuition)
qqline(college$tuition, col="red")
#looks fairly normal, but since number of observation is above 30, we assume normality.


# check for equal variances
var.test(tuition~type,data=college)
#p:2.2e-16, we reject the null hypothesis that the variance is equal.

# t-test with unequal variances
t.test(tuition~type,var.equal=FALSE,paired = FALSE,data=college)
#p:2.2e-16, we reject the null hypothesis, thus there exists significant difference in tuition by type of institution and Private non profit institutes have higher tuition as indicated by the higher mean of 28301.69 compare to Public institutes with mean tuition of 18855.88



###--------------------------------------Question 3--------------------------------------###

happy = na.omit(happy)
count(happy,'Region')

boxplot(Hscore~Region, data=happy)
#there seems to be a difference in medians of Hscore for different regions.

# normality check
hist(happy$Hscore)
shapiro.test(happy$Hscore)
#p:0.01248 < 0.05, we reject the null hypothesis that the data is normal.
qqnorm(happy$Hscore)
qqline(happy$Hscore,col="red")
#looks normal, but since number of observation is above 30, we assume normality.

# check for equal variance
leveneTest(Hscore~Region, data=happy)
#p:0.5179 > 0.05, accept the null hypothesis and the variance of Hscore for different regions is not different.

# anova
anova(aov(Hscore~Region,data=happy))
#p:1.279e-12, We reject the null hypothesis, thus Hscore varies for different region.

TukeyHSD(aov(Hscore~Region,data=happy))
# There exists a significant difference of Hscore between AmericasCarribean-AfricaMideast,AsiaAustralia-AfricaMideast,Europe-AfricaMideast and AsiaAustralia-AmericasCarribean. 
plot(TukeyHSD(aov(Hscore~Region,data=happy)))

###--------------------------------------Question 4--------------------------------------###

#checking for correlations
happy_continuos_covariates = subset(happy,select = -c(Country, Region ))

cor(happy_continuos_covariates,happy_continuos_covariates)
pairs(happy_continuos_covariates,happy_continuos_covariates)

model=lm(Corruption~Generosity+Hscore+GDP,data=happy)
vif(model)

#Check normality for y variable - before transformation
hist(happy$Corruption)
shapiro.test(happy$Corruption)
#p:5.128e-1 < 0.05, we reject the null hypothesis that the data is normal.
qqnorm(happy$Corruption)
qqline(happy$Corruption,col="red")
#doesnt looks normal

#Applying log transformation
log_corruption = log(happy$Corruption) 
#Check normality for y variable - after transformation
hist(log_corruption)
shapiro.test(log_corruption)
#p:0.08535 > 0.05, we accept the null hypothesis that the data is normal.
qqnorm(log_corruption)
qqline(log_corruption,col="red")
#looks normal

#Linear relationship
plot(log_corruption~Generosity, dat=happy)
abline(lm(log_corruption~Generosity, dat=happy), col="red")

plot(log_corruption~GDP, dat=happy)
abline(lm(log_corruption~GDP, dat=happy), col="red")

plot(log_corruption~Hscore, dat=happy)
abline(lm(log_corruption~Hscore, dat=happy), col="red")

#Linear model
model_corruption=lm(log_corruption~Generosity+Hscore+GDP,data=happy)
summary(model_corruption)

#Checking for model fit
dwtest(model_corruption,alternative=c("two.sided"))
#p-value: 0.286 > 0.05, since p-value is greater than 0.05, the null hypothesis is true and there doesnt exists autocorrelation and the residuals are indeppendent.

shapiro.test(residuals(model_corruption))
#p-value = 0.001588 < 0.05, The residuals are not normally distributed.
qqnorm(residuals(model_corruption))
qqline(residuals(model_corruption),col="red")
#But the qqplot show that the residuals are normally distributed.

bptest(model_corruption)
#p:0.05496, the residuals are homostedastic.

plot(model_corruption)



###--------------------------------------Question 5--------------------------------------###
model_corruption_region=lm(log_corruption~Generosity+Region+Generosity*Region,data=happy)
summary(model_corruption_region)

#Checking for model fit
dwtest(model_corruption_region,alternative=c("two.sided"))
#p-value: 0.9724 > 0.05, since p-value is greater than 0.05, the null hypothesis is true and there doesnt exists autocorrelation and the residuals are indeppendent.

shapiro.test(residuals(model_corruption_region))
#p-value = 0.1177 < 0.05, The residuals are not normally distributed.
qqnorm(residuals(model_corruption_region))
qqline(residuals(model_corruption_region),col="red")
#But the qqplot show that the residuals are normally distributed.

bptest(model_corruption_region)
#p:0.01836<0.05, the residuals are not homostedastic.

plot(model_corruption_region)


###--------------------------------------Question 6--------------------------------------###

cancer = na.omit(cancer)

#checking for correlations
cor(cancer,cancer)
pairs(cancer,cancer)

model=lm(malignant~smoothness_mean+texture_mean+radius_mean,data=cancer)
vif(model)

#Check normality for y variable
hist(cancer$malignant)
shapiro.test(cancer$malignant)
#p:<2.2e-16 < 0.05, we reject the null hypothesis that the data is normal.
qqnorm(cancer$malignant)
qqline(cancer$malignant,col="red")
#doesnt looks normal at all

#Using GLM with binomial distribution and and logit link function
model_malign = glm(malignant~smoothness_mean+texture_mean+radius_mean,data=cancer,family='binomial') # default is the 'logit' link
summary(model_malign)

#Checking for model fit
#dwtest(model_malign,alternative=c("two.sided"))
#p-value: 3.616e-05 < 0.05, since p-value is lesser than 0.05, the null hypothesis is rejected and there exists autocorrelation and the residuals are not indeppendent.

#shapiro.test(residuals(model_malign))
#p-value <2.2e-16 < 0.05, The residuals are not normally distributed.
#qqnorm(residuals(model_malign))
#qqline(residuals(model_malign),col="red")
#The qqplot show that the residuals are not normally distributed.

#bptest(model_malign)
#p:4.533e-10, the residuals are not homostedastic.

plot(model_malign)

model_malign0 = glm(malignant~1,data=cancer,family='binomial') # default is the 'logit' link
summary(model_malign0)

anova(model_malign0, model_malign, test="Chi")
AIC(model_malign0, model_malign)

###--------------------------------------Question 7--------------------------------------###

model_malign1 = glm(malignant~smoothness_mean,data=cancer,family='binomial') # default is the 'logit' link
summary(model_malign1)

model_malign2 = glm(malignant~smoothness_mean+radius_mean,data=cancer,family='binomial') # default is the 'logit' link
summary(model_malign2)

model_malign3 = glm(malignant~smoothness_mean+radius_mean+texture_mean,data=cancer,family='binomial') # default is the 'logit' link
summary(model_malign3)

anova(model_malign1,model_malign2,model_malign3,test="Chi")
AIC(model_malign1,model_malign2,model_malign3)



