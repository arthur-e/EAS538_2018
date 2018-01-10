flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

library(dplyr)
library(car)
library(lmtest)

### Question 1 ###
head(flying)
tbl = table(flying$unruly_child, flying$gender) 
tbl
chisq.test(tbl)

### Question 2 ###
head(college)

## Checking Assumptions ##
leveneTest(tuition~type, data=college)
shapiro.test(college[,"tuition"])
# Data are not normally distributed and variances are unequal.

## Transforming the data ##
college = college %>%
  mutate(tuition.sqrt=sqrt(tuition))
leveneTest(tuition.sqrt~type, data=college)
shapiro.test(college[,"tuition.sqrt"])
# Data are not normally distributed and variances are unequal.

college = college %>%
  mutate(tuition.log=log(tuition))
leveneTest(tuition.log~type, data=college)
shapiro.test(college[,"tuition.log"])
# Data are not normally distributed and variances are unequal.

public = subset(college, type=="Public")
private = subset(college, type=="Private nonprofit")

## Assume normality by the CLT. Unequal variances means I will conduct a Welch's t-test.
t.test(public$tuition.log, private$tuition.log)
# There is a significant (p-value = 2.2e-16) difference in tuition between private and public schools

### Question 3 ###
head(happy)

## Checking Assumptions ##
shapiro.test(happy[,"Hscore"])
leveneTest(Hscore~Region, data=happy)
# The data are not normally distributed (p-value = 0.0145)
# The data do have equal variance (p-vlaue = 0.5179)

## Transforming the data ##
happy = happy %>%
  mutate(Hscore.sqrt=sqrt(Hscore))
shapiro.test(happy[,"Hscore.sqrt"])
# Data are not normally distributed

happy = happy %>%
  mutate(Hscore.log=log(Hscore))
shapiro.test(happy[,"Hscore.log"])
# Data are not normally distributed

## Assume normality by the CLT. Equal variances means I will conduct an ANOVA
aov = aov(Hscore~Region, data=happy)
summary(aov)
# There is a significant (p-value = 1.28e-12) difference in happiness between the various regions

### Question 4 ###

## I am going to include GDP, Generosity, and Freedom in my model

## Checking for normality
shapiro.test(happy[,"Corruption"])
# The dependent variable is not normally distributed (p-value=5.128e-11)
happy = happy %>%
  mutate(Corruption.log=log(Corruption))
shapiro.test(happy[,"Corruption.log"])
# Log transforming the data results in a normally distributed dependent variable (p-value = 0.08535)

## Checking for collinearity
cor(happy[,c("GDP", "Freedom", "Generosity")], use="na.or.complete")
# The Pearson correlation coefficients are below 0.40 so there shouldn't be a problem with collinearity.
mod = lm(Corruption.log~GDP + Life + Freedom, data=happy)
vif = 1/(1 - summary(mod)$r.squared)
vif
# The VIF is only 1.258928. Because this value is less than 3, it is fine to include all three independent variables.

## Checking for a linear relationship
plot(Corruption.log~GDP, data=happy, main="Relationship between corruption and GDP")
plot(Corruption.log~Freedom, data=happy, main="Relationship between corruption and Freedom")
plot(Corruption.log~Generosity, data=happy, main="Relationship between corruption and Generosity")
# It appears that the transformed dependent variable and the independent variables have a linear relationship

## After checking that the assumptions are met, I will run the model
summary(mod)
# When the GDP, Life, and Freedom are all zero, Corruption is -3.0495 units.
# The intercept is significantly (p-value = 2e-16) different from 0. 
# The relationship between GDP and Corruption is not significant (p-value = 0.536).
# The relationship between Life and Corruption is not significant (p-value = 0481).
# The relationship between Freedom and Corruption is  significant (p-value = 3.07e-08), such that when Freedom increases by 1 unit, Corruption increases by 2.4032 units 
# The adjusted R-squared value indicates that the model explains 19.01% of the variation in Corruption. 
# The null model (i.e. the model with the intercept only) can be rejected (p-value = 1.031e-07). Thus, including the other independent variables in the full model explains significantly more of the variation in Corruption than the null model alone.

## Model Checking 
# Checking independence of residuals
dwtest(mod, alternative=c("two.sided"))
# Fail to reject null hypothesis that errors are independent
# p-value = 0.3066
# Errors are independent

# Checking for homoscedasticity
bptest(mod)
# Fail to reject null hypothesis that errors are homoscedastic
# p-value = 0.5552
# Errors are homoscedastic

# Checking for normality
shapiro.test(residuals(mod))
# Reject null hypothesis that errors are normally distributed
# p-value = 0.002568
# Errors are not normal


### Question 5 ###

mod.1 = lm(Corruption.log~Freedom*Region, data=happy)
summary(mod.1)
# When Freedom zero, Corruption in AfricaMideast is log -2.6096 units. Also, the intercept is significantly (p-value = 2e-16) different from 0. 
# The relationship between Freedom and Corruption in AfricaMideast is significant (p-value = 0.009360), such that when Freedom increases by 1 unit in AfricaMideast, Corruption increases by log 1.5929 units. 
# Controlling for Freedom, Corruption in Europe is log 1.3755 units lower than in AfricaMideast, and this difference is significant (p-value = 2.42e-5). 
# The effect of Freedom on Corruption is significantly (p-value = 0.000494) different in Europe than it is in AfricaMideast. 
# The adjusted R-squared value indicates that the model explains 30.53% of the variation in Corruption. 
# The null model (i.e. the model with the intercept only) can be rejected (p-value = 5.739e-11). Thus, including the other independent variables in the full model explains significantly more of the variation in Corruption than the null model alone. 


### Question 6 ###
cor(cancer)
# Texture, Area, and Smoothness will be included in the model because they are not highly correlated with each other. 

mod.2 = glm(malignant~texture_mean + area_mean + smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(mod.2)
# When Texture, Area, and Smoothness are all zero, the log odds of a tumor being malignant is -32.619261, which is significantly (p-value = 2e-16) different from zero. 
# For each unit increase in texture, the log odds of a tumor being malignant increases by 0.381076, which is significant (p-value=3.92e-11). 
# For each unit increase in area, the log odds of a tumor being malignant increases by 0.016260, which is significant (p-value=2e-16). 
# For each unit increase in smoothness, the log odds of a tumor being malignant increases by 146.766602, which is significant (p-value=2.25e-14).

### Question 7 ###
smooth = glm(malignant~smoothness_mean, data=cancer, family=binomial(link="logit"))
area = glm(malignant~area_mean, data=cancer, family=binomial(link="logit"))
text = glm(malignant~texture_mean, data=cancer, family=binomial(link="logit"))

anova(mod.3, mod.4, mod.5, test="Chi")
# The ANOVA indicates that there is no significant difference between the likelihood of the three models

AIC(smooth, area, text)
# AIC for mod2 = 3936.668
# AIC for mod1 = 4012.673
# Because mod2 (the model that includes dist100) has a lower AIC score, we know that including dist100 as an independent variable increases the likelihood of the model




