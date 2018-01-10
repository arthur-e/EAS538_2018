##Read in Data
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

##Question 1: Is there a significant association between gender (gender) and whether people think it's rude to bring an unruly child on the plane (unruly_child)? If yes, which gender tends to think that bringing an unruly child is more rude?
##Chi Square
##Assumptions: independent observations, observations in all cells of contingency table, and random samples
  ##I Assume all independent observations based on the fact that each respondent has a unique ID, and that they were randomly selected, though there is no way to verify these assumptions for certain.
  ##If we summed the data, we would have observations in all possible cells of the contingency table (i.e. males who answered no, somewhat, and very, and females who answered no, somewhat, and very).
##Running the test
chisq.test(flying$gender, flying$unruly_child)
  ##The p-value of the chi-square test is 0.001193, so we can reject the null hypothesis that there is not an association between gender and whether people think it's rude to bring an unruly child on the plane.
  ##Therefore, there is a significant association between gender and whether people think it's rude to bring an unruly child on the plane.
male = subset(flying, gender=="Male")
female = subset(flying, gender =="Female")
percent_male_rude = (length(which(male$unruly_child=="Somewhat")) + length(which(male$unruly_child=="Very")))/479
percent_female_rude = (length(which(female$unruly_child=="Somewhat")) + length(which(female$unruly_child=="Very")))/528
  ##To determine which gender thought it was more rude to bring an unruly child on the plane, for each gender I summed the number of individuals who answered "somewhat" and "very", and divided it by the total number of individuals.
  ##Based on percentages of those surveyed, men think it is more rude


##Question 2: Is there a significant difference in tuition (tuition) by type of institution (type)? If yes, which type has a higher tuition?
##Two sample, one tailed T-test
##Assumptions: Normal dependent variable distribution, equal variances, randomly selected samples, independent observations.
  ##I assume all independent observations and that they were randomly selected, though there is no way to verify these assumptions for certain.
##Test for normality
library(car)
hist(college$tuition)
qqPlot(college$tuition)
shapiro.test(college$tuition)
  ##The p-value from the shapiro test is 3.193e-15, so we can reject the null hypothesis that the distribution of tuition is normal.
  ##However, the distribution of tuition looks close to normal from the histogram and qqPlot.
  ##Furthermore, we have a large enough sample size to assume normality.
##Test for equal variance
Private_NP = subset(college, type =="Private nonprofit")
Public = subset(college, type == "Public")
var.test(Private_NP$tuition, Public$tuition)
  ## The p value from the var.test function is 2.2e-16, which is less than 0.05, so we reject the H0 of this test that the variances are equal (i.e. the variances are not equal)
  ## It is ok, because we can use Welch's T-test
##Running the test
t.test(Private_NP$tuition, Public$tuition, alternative = "greater")
  ##The p-value is 2.2e-16, so we can reject the null hypothesis that the mean of x is less than or equal to the mean of y.
  ##Therefore, there is a significant difference in tuition by type of institution.
  ##The mean of x is greater than the mean of y, so Private Non-Profit schools have a higher tuition than Public schools.


##Question 3: Is there a significant difference in happiness (Hscore) by region (Region)?
##ANOVA
##Assumptions: Normal dependent variable distribution, equal variances, randomly selected samples, and independent samples.
  ##I assume all indpendent observations and that they were randomly selected, though there is no way to verify these assumptions for certain.
##Test for normality
hist(happy$Hscore)
qqPlot(happy$Hscore)
shapiro.test(happy$Hscore)
  ##The p-value from the shapiro test is 0.01248, so we can reject the null hypothesis that the distribution of tuition is normal.
  ##However, the distribution of Hscore looks close to normal from the histogram and qqPlot.
  ##Only the tails deviate significantly from the qqPlot, so I am going to assume normality.
##Test for equal variance
leveneTest(Hscore~Region, data = happy, center = mean)
  #The p-value from the levene test is 0.342, which is greater than 0.05, so we fail to reject the null hypothesis of this test that the variances are equal (i.e. the variances are equal)
Region.aov = aov(Hscore~Region, data = happy)
summary(Region.aov)
  #The p-value from the anova summary is 1.28e-12, which is less than 0.05, so we reject the null hypothesis that Hscore is not different based on region.
  #Happiness differs based on region.


##Question 4: What factors are significantly associated with a country's corruption levels (Corruption)? Choose three continuous independent variables to include in your model. 
##Linear Model
##Assumptions: normal dependent variable distribution, linear relationship, residual independency, and homoscedasticity.
##Test for normality
hist(happy$Corruption)
qqPlot(happy$Corruption)
shapiro.test(happy$Corruption)
  ##The p-value from the shapiro test is 5.128e-11, so we can reject the null hypothesis that the distribution of corruption is normal.
  ##The distribution is also skewed judging from the histogram and qqPlot, so it should be transformed.
library(magrittr)
library(plyr)
library(reshape2)
##Transforming Corruption
happy1 = happy %>%  
  mutate(logCorruption = log(Corruption))
hist(happy1$logCorruption)
qqPlot(happy1$logCorruption)
shapiro.test(happy1$logCorruption)
  ##After log transforming corruption, the p value from the shapiro test is 0.08535, so we fail to reject the null hypothesis that the distribution of corruption is normal.
  ##The distribution of log transformed corruption also looks normal from the histogram and qqPlot.

##Selecting independent variables
cor(happy[, c("Hscore", "GDP", "Family", "Life", "Freedom", "Corruption", "Generosity")])
vif.GDP = 1/(1 - summary(lm(GDP~Generosity + Freedom, dat=happy))$r.squared)
vif.Generosity = 1/(1 - summary(lm(Generosity~GDP + Freedom, dat=happy))$r.squared)
vif.Freedom = 1/(1 - summary(lm(Freedom~GDP + Generosity, dat=happy))$r.squared)
##Chosen model: corruption~GDP+Generosity+Freedom
##Linear relationship: I assume a linear relationship between the dependent variable and these independent variables
plot(happy1$GDP, happy1$logCorruption)
plot(happy1$Generosity, happy1$logCorruption)
plot(happy1$Freedom, happy1$logCorruption)
##cor(GDP, Generosity) = -0.02553066
##cor(GDP, Freedom) = 0.36228285
##cor(Generosity, Freedom) = 0.3617513
##vif.GDP = 1.189712
##vif.Freedom = 1.367952
##vif.Generosity = 1.189185
##Running the model
mod = lm(logCorruption ~ GDP + Freedom + Generosity, data = happy1)
summary(mod)
#Residual independency
install.packages("lmtest")
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
plot(residuals(mod))
  ##The p-value from the dwtest is 0.3701, so we fail to reject the null hypothesis of this test that there is not autocorrelation in the residuals.
  ##The plot of the residuals also looks to have residual independency.  
  ##Therefore, the assumption of residual independency is not violated.
#homoscedasticity
bptest(mod)
  ##the bp test returns a p-value of 0.06878, so we fail to reject the null hypothesis of this test that there is not heteroscedasticity.
  ##Heteroscedasticity is also not visible from the plot of the residuals.  
  ##Therefore, the assumption of residual homoscedasticity is not violated.
#Normal errors
shapiro.test(residuals(mod))
hist(residuals(mod))
qqPlot(residuals(mod))
  ##The p-value from the shapiro test is 0.0006382, so we reject the null hypothesis that the errors are normally distributed.
  ##Therefore, the errors are not normally distributed according to the shapiro test.
  ##However, judging from the qqPlot the distribution is close to normal, so I am assuming the distribution of errors is actually normal.


##Question 5: Choose one of the continuous independent variables that was significant in the model for Question 4 and interact it with region (Region) to predict corruption (Corruption). This model should only include one continuous independent variable and its interaction with region. Does the influence of your continuous variable on corruption vary by region? If yes, how do you interpret the interaction?
##Chosen model: corruption~Freedom*Region
mod1 = lm(logCorruption ~ Freedom*Region, data = happy1)
summary(mod1)
#Residual independency
dwtest(mod1, alternative = c("two.sided"))
plot(residuals(mod1))
  ##The p-value from the dwtest is 0.5627, so we fail to reject the null hypothesis of this test that there is not autocorrelation in the residuals.
  ##The plot of the residuals also looks to have residual independency.  
  ##Therefore, the assumption of residual independency is not violated.
#homoscedasticity
bptest(mod1)
  ##the bp test returns a p-value of 0.2501, so we fail to reject the null hypothesis of this test that there is not heteroscedasticity.
  ##Heteroscedasticity is also not visible from the plot of the residuals.  
  ##Therefore, the assumption of residual homoscedasticity is not violated.
#Normal errors
shapiro.test(residuals(mod1))
hist(residuals(mod1))
qqPlot(residuals(mod1))
  ##The p-value from the shapiro test is 0.01481, so we reject the null hypothesis that the errors are normally distributed.
  ##Therefore, the errors are not normally distributed according to the shapiro test.
  ##However, judging from the qqPlot the distribution is close to normal, so I am assuming the distribution of errors is actually normal.

##Interpretation
  ##The influence of freedom does vary by region. The coefficient for Freedom means that the effect of freedom on log of corruption is significant for RegionAfricaMideast. Every unit increase in freedom leads to a change of the log of corruption of -2.6096 in RegionAfricaMideast. 
  ##The RegionAmericasCarribean and RegionAsiaAustralia variables are not significant, so this means that the intercept for these variables are the same as RegionAfricaMideast. These varibles do not have an effect on log of corruption when freedom is set to 0. 
  ##The RegionEurope variable is significant, so this means that the intercept for Europe changes by log(-1.3755) relative to log(-2.9096) when freedom is set to 0. 
  ##The coefficient for Freedom:Region Europe is significant, and means that the slope for the effect of freedom on log of corruption is significantly different for RegionEurope than any other region. The slope (or effect of freedom on log of corruption) for Europe is log(1.5929) + log(2.9826).


##Question 6:	Which factors are significantly associated with whether a breast cancer tumor is malignant or not? Choose three continuous independent variables to include in your model. cancer
cor(cancer[, c("radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean")])
mod2 = glm(malignant~area_mean + texture_mean + smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(mod2)
##Interpretation: when malignant is 0, the log(odds ratio) is equal to -32.619261
##Increase 1 unit area, log(odds ratio) increases by 0.016260
##Increase 1 unit texture, log(odds ratio) increase by 0.381076
##Increase 1 unit smoothness, log (odds ratio) increases by 146.766602
##The output shows that when we added the variables, and reduced our degrees of freedom, the model has less deviance than a random model.

##Question 7: BONUS/EXTRA CREDIT: Which independent variables are the most important in explaining whether a breast cancer tumor is malignant or not? Use the same 3 continuous independent variables you chose for question 6.
Area_Mean_Effect = exp(0.016260)
Texture_Mean_Effect = exp(0.381076)
Smoothness_Mean_Effect = exp(146.766602)
Area_Mean_Effect
Texture_Mean_Effect
Smoothness_Mean_Effect
##Smoothness is the most important in explaining whether a breast cancer turmor is malignant or not.
