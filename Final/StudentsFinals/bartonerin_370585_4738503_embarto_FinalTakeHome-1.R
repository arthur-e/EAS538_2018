#Erin Barton
#23 APril 2017
#Final Take Home

#1:
#check assumptions that all cells are filled. 
table1 <- table(flying$unruly_child, flying$gender)
table1
#         Female Male
#No           91   56
#Somewhat    193  155
#Very        158  190

#run test
chisq.test(flying$unruly_child, flying$gender)
#Pearson's Chi-squared test
#data:  flying$unruly_child and flying$gender
#X-squared = 13.463, df = 2, p-value = 0.001193

#The results show that there is a significant association between gender and whether you think it’s rude to bring an unruly child on the plane. Men tend to think it is very rude than women do, although women are tend to think it is somewhat rude more than men do. Women tend to think it is not rude at all more so than men

#2:
#Hypothesis
#H0: There is no significant difference in tuition based on type of institution
#Ha: There is a significant difference in tuition based on type of institution. 

#Variables
#Tuition: continuous, dependent variable
#Type of Institution: categorical, independent variable

#Test: Two-sample T-Test
priv.college <- subset(college, type=="Private nonprofit")
pub.college <- subset(college,type=="Public")
#assumptions:
#Population>30, which means normality can be assumed. 
var.test(priv.college$tuition, pub.college$tuition)
#F test to compare two variances
#data:  priv.college$tuition and pub.college$tuition
#F = 2.3005, num df = 871, denom df = 534, p-value < 2.2e-16
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  1.97238 2.67567
#sample estimates:
#  ratio of variances 
#2.300516 

#Transformation: A Welch’s Two Sample T-Test should be run instead of a regular t-test as the variance between groups is not equal. 

#run test
t.test(priv.college$tuition, pub.college$tuition, paired=FALSE)
#Welch Two Sample t-test
#data:  priv.college$tuition and pub.college$tuition
#t = 22.79, df = 1397.9, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  8632.749 10258.878
#sample estimates:
#  mean of x mean of y 
#28301.69  18855.88 

#check to see which type has higher tuition
hist(priv.college$tuition, breaks=15, col="light blue", xlab="tuition", main="")
abline(v=mean(priv.college$tuition), col="blue")
par(new=TRUE)
hist(pub.college$tuition, breaks=15, col="light green", xlab="", main="")
abline(v=mean(pub.college$tuition), col="dark green")

#This test shows that Ha is supported: there is a significant difference in tuition by type of institution. This can be seen by how the p-value is less than .05. Using a histogram plot, we can see that the mean tuition of private colleges is higher than the mean tuition of public nonprofit colleges. 


#3: 
#Hypothesis: 
#H0: There is not a significant difference in Hscore by region.
#Ha: There is a significant difference in Hscore by region.
#Variables: 
#Happiness: continuous, dependent
#Region: categorical (4 levels), independent

#Test: One-Way ANOVA
#visualize the data:
boxplot(Hscore~Region, data=happy, xlab="Region", ylab="Happiness Score")

#assumptions
#normality <- N>30
#equal variance
library(car)
leveneTest(Hscore~Region, data=happy)
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group   3  0.7605 0.5179
#153    

#p-value>.05, indicating that the population is of equal variance. 


#Run test
modaov <- aov(Hscore~Region, data=happy)
summary(modaov)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
#Region        3  64.37  21.456   23.62 1.28e-12 ***
#Residuals   153 138.96   0.908                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#The aov test shows that there is a significant difference in happiness by region. 


#Run post-hoc test
TukeyHSD(modaov)
#Tukey multiple comparisons of means
#95% family-wise confidence level
#Fit: aov(formula = Hscore ~ Region, data = happy)
#$Region
#                                  diff         lwr         upr     p adj
#AmericasCarribean-AfricaMideast  1.5397193  0.94745533  2.13198329 0.0000000
#AsiaAustralia-AfricaMideast      0.7472543  0.14642988  1.34807874 0.0081572
#Europe-AfricaMideast             1.3208593  0.84313961  1.79857901 0.0000000
#AsiaAustralia-AmericasCarribean -0.7924650 -1.49989372 -0.08503628 0.0214193
#Europe-AmericasCarribean        -0.2188600 -0.82522748  0.38750748 0.7847082
#Europe-AsiaAustralia             0.5736050 -0.04112656  1.18833656 0.0768934

#The Tukey’s HSD test suggests that the relationship between happiness and region is not significant for Europe-AmericasCarribean and Europe–AsiaAustralia, but it is significant between all other groups. 


#4:
#Hypothesis: 
#H0: Corruption is not significantly associated with Hscore, Generosity or GDP.
#Ha: Corruption is significantly associated with Hscore, Generosity or GDP.

#Variables:
#Corruption levels: continuous, dependent variable
#Hscore, Generosity, GDP: continuous, independent variables

#Test: Multiple Regression
#check colinearity
df <- data.frame(happy$Corruption, happy$Hscore, happy$Generosity, happy$GDP)
cor(df)
plot(df)
library(car)
vif(lm(Corruption~Hscore+Generosity+GDP, data=happy))
#The VIF shows that none of the selected variables are highly correlated as vif<5 for all of them. 
#plot histogram of y variables
hist(happy$Corruption)
#look at qqplot
qqnorm(happy$Corruption)
#The histogram and qqplot indicate that the data is not normally distributed and should be transformed

#transform
logCorruption <- log(happy$Corruption)
hist(logCorruption)
qqnorm(logCorruption)
qqline(logCorruption, col="red")

sqCorruption <- sqrt(happy$Corruption)
hist(sqCorruption)
qqnorm(sqCorruption)
qqline(sqCorruption, col="red")

#The histogram and qqplot suggest that the log transformation made the data more normal and good enough to run through the regression analysis. 


#run test
mod1 <- lm(logCorruption~Hscore+Generosity+GDP, data=happy)
summary(mod1)
#The model indicates that there is a significant positive relationship between the log of corruption and happiness, and the log of corruption and generosity. So When happiness score goes up one unit, corruption increases by the log of 0.2614, and the same is true of Generosity. There is not significant relationship between the log of corruption and GDP. 


#Model Checking
plot(df)
#A plot of the variables shows the relationships are mostly linear. 
#independent errors
library(lmtest)
dwtest(mod1, alternative=c("two.sided"))
#Durbin-Watson test
#data:  mod1
#DW = 1.8485, p-value = 0.286
#alternative hypothesis: true autocorrelation is not 0
#The Durbin Watson test returns a p-value>.05, indicating that the errors are independent. 


#homoscedasticity
plot(residuals(mod1)~fitted(mod1))
abline(lm(residuals(mod1)~fitted(mod1)), col="red")
bptest(mod1)
#studentized Breusch-Pagan test
#data:  mod1
#BP = 7.6033, df = 3, p-value = 0.05496
#The Bresch-Pagan test shows that there is a borderline possibility of heteroscedasticity as the p-value = .055. A plot of the residuals, though, shows that there does not seem to be a problem with heteroscedasticity in the errors. 


#normality of errors
qqnorm(residuals(mod1))
qqline(residuals(mod1), col="red")
hist(residuals(mod1))
shapiro.test(residuals(mod1))
#A qqplot and a histogram both show that the errors appear nearly normal, even though the shapiro.wilkes test suggest that the errors are not normal (p-value<.05)


#Model Fitting
mod2 <- lm(logCorruption~Hscore+Generosity, data=happy)
summary(mod2)
AIC(mod1, mod2)
#Based on the check of the assumptions, the model is a decent fit for the data. It does not grossly violate any assumptions even if it does not fit within the bounds of the more conservative tests, such as the Shapiro-Wilkes test. The adjusted R-Squared value, however, is not very high. It suggests that the model only explains 12% of the variation. To check and see if this can be higher, the insignificant GDP can be taken out. This results in an R-Squared of .123, which is not any better than the other model. The AIC test also shows no real difference in the two models. 



#5:
summary(mod1)
mod3 <- lm(logCorruption~Hscore*Region, data=happy)
summary(mod3)
#The model shows that the influence of happiness score does vary by region. The effect of Hscore on corruption increases in Europe compared to Africa and the Middle East by the log of 0.46548. 

library(interplot)
interplot(m=mod3, var1="Hscore", var2="Region") + labs(x="Region", y="Estimated Coeffiecient for Happiness Score")
interplot(m=mod3, var1="Region", var2="Hscore")
#An interplot further shows that the effect of Hscore on the log of Corruption is only significant in Europe



#5:
#Hypothesis: 
#H0: Mean Radius, Mean Texture, Mean Smoothness are not significantly associated with whether a breast cancer tumor is malignant or not. 
#Ha: Mean Radius, Mean Texture, Mean Smoothness are significantly associated with whether a breast cancer tumor is malignant or not.

#Variables:
#malignant: binomial categorical, dependent variable
#Mean Radius, Mean Texture, Mean Smoothness: continuous, independent variables

#Test: GLM-Binomial 
#check assumptions
vif(glm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer, family='binomial'))
cor(cancer)
#The VIF shows that none of the selected variables are highly correlated as vif<5 for all of them. The cor(cancer) function also reveals no collinearity.
hist(cancer$malignant)
#The histogram suggests that the y variable is binomial, supporting the use of a binomial GLM. 

#run test
glm_mod = glm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(glm_mod)
#This model shows that radius, texture, and smoothness are all significant in determining the probability that a tumor will be malignant. So the coefficient of the mean radius is the effect of mean radius on the probability of tumor malignancy, and the same for texture and smoothness. By increasing on unit of mean radius, the log odds of malignancy increase by 1.397. 


#check model fit
mod0 = glm(malignant~1, data=cancer, family=binomial(link=logit))
AIC(mod0,glm_mod)
#The AIC model shows that the glm model has a lower AIC score than the null model (mod0). This indicates it is a better fit for the data. 


#Bonus
#demean datat to show which variables are most significant
library(dplyr)
glm_mod = glm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(glm_mod)
cancer=mutate(cancer,radius_mean=(radius_mean-mean(cancer$radius_mean))/sd(cancer$radius_mean),texture_mean=(texture_mean-mean(cancer$texture_mean))/sd(cancer$texture_mean),smoothness_mean=(smoothness_mean-mean(cancer$smoothness_mean))/sd(cancer$smoothness_mean))
summary(glm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit")))
#This scaled model shows that mean radius affects the log odds of whether or not a tumor is malignant the most. 


