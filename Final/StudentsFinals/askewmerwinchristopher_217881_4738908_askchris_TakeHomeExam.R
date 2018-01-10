# Final Take-Home Exam
# Christopher Askew-Merwin
# 4/22/2017

library(car)

flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

#_____________________________________________________________________________
# Question 1

tbl = table(flying$gender,flying$unruly_child)
#         No Somewhat Very
# Female  91      193  158
# Male    56      155  190

chisq.test(tbl)
# Pearson's Chi-squared test
#
# data:  tbl
# X-squared = 13.463, df = 2, p-value = 0.001193

#_____________________________________________________________________________
# Question 2

plot(college$type, college$tuition)

private = subset(college, type=="Private nonprofit")
public = subset(college, type=="Public")

var.test(private$tuition, public$tuition)
# F test to compare two variances
#
# data:  private$tuition and public$tuition
# F = 2.3005, num df = 871, denom df = 534, p-value < 2.2e-16
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  1.97238 2.67567
# sample estimates:
# ratio of variances 
#           2.300516

t.test(private$tuition, public$tuition)
#      Welch Two Sample t-test
#
# data:  private$tuition and public$tuition
# t = 22.79, df = 1397.9, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  8632.749 10258.878
# sample estimates:
# mean of x mean of y 
#  28301.69  18855.88 

#_____________________________________________________________________________
# Question 3

leveneTest(Hscore ~ Region, data=happy)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group   3  0.7605 0.5179
#      153           

plot(Hscore ~ Region, data=happy)

happymodel = aov(Hscore ~ Region, data=happy)
summary(happymodel)
#              Df Sum Sq Mean Sq F value   Pr(>F)    
# Region        3  64.37  21.456   23.62 1.28e-12 ***
# Residuals   153 138.96   0.908                     
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

TukeyHSD(happymodel)
# Tukey multiple comparisons of means
#   95% family-wise confidence level
#
# Fit: aov(formula = Hscore ~ Region, data = happy)
#
# $Region
#                                       diff         lwr         upr     p adj
# AmericasCarribean-AfricaMideast  1.5397193  0.94745533  2.13198329 0.0000000
# AsiaAustralia-AfricaMideast      0.7472543  0.14642988  1.34807874 0.0081572
# Europe-AfricaMideast             1.3208593  0.84313961  1.79857901 0.0000000
# AsiaAustralia-AmericasCarribean -0.7924650 -1.49989372 -0.08503628 0.0214193
# Europe-AmericasCarribean        -0.2188600 -0.82522748  0.38750748 0.7847082
# Europe-AsiaAustralia             0.5736050 -0.04112656  1.18833656 0.0768934

#_____________________________________________________________________________
# Question 3

qqnorm(happy$Corruption)
hist(happy$Corruption)

happy$logCorruption = (log(happy$Corruption))

qqnorm(happy$logCorruption)
qqline(happy$logCorruption)
hist(happy$logCorruption)

happy$sqrtCorruption = (sqrt(happy$Corruption))

qqnorm(happy$sqrtCorruption)
qqline(happy$sqrtCorruption)
hist(happy$sqrtCorruption)


happy$cubCorruption = (sign(happy$Corruption)*abs(happy$Corruption)^(1/3))

qqnorm(happy$cubCorruption)
qqline(happy$cubCorruption)
hist(happy$cubCorruption)

cor(happy[c("Freedom","Generosity","GDP")], use="na.or.complete")
#              Freedom  Generosity         GDP
# Freedom    1.0000000  0.36175133  0.36228285
# Generosity 0.3617513  1.00000000 -0.02553066
# GDP        0.3622828 -0.02553066  1.00000000

vif(lm(cubCorruption~Freedom+Generosity+GDP, data=happy))
#  Freedom Generosity        GDP 
# 1.367952   1.189185   1.189712

cormodel = lm(cubCorruption~Freedom+Generosity+GDP, data=happy)

summary(cormodel)
# Call:
# lm(formula = cubCorruption ~ Freedom + Generosity + GDP, data = happy)
#
# Residuals:
#       Min        1Q    Median        3Q       Max 
# -0.309129 -0.074914 -0.001362  0.080485  0.259579 
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.31657    0.02937  10.778  < 2e-16 ***
# Freedom      0.34693    0.07035   4.932  2.1e-06 ***
# Generosity   0.12371    0.07135   1.734    0.085 .  
# GDP          0.02415    0.02314   1.044    0.298    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 0.1093 on 153 degrees of freedom
# Multiple R-squared:  0.2488,	Adjusted R-squared:  0.2341 
# F-statistic: 16.89 on 3 and 153 DF,  p-value: 1.574e-09

plot(cormodel)

library(lmtest)

dwtest(cormodel, alternative=c("two.sided"))
#        Durbin-Watson test
#
# data:  cormodel
# DW = 1.8087, p-value = 0.1932
# alternative hypothesis: true autocorrelation is not 0

bptest(cormodel)
#       studentized Breusch-Pagan test
#
# data:  cormodel
# BP = 8.2748, df = 3, p-value = 0.04066

qqnorm(residuals(cormodel))
qqline(residuals(cormodel), col="red")

shapiro.test(residuals(cormodel))
#     Shapiro-Wilk normality test
#
# data:  residuals(cormodel)
# W = 0.99399, p-value = 0.7654

#____________________________________________________________________
# Question 5

library(Lahman)
library(dplyr)
library(ggplot2)

ancovamod = lm(cubCorruption~Freedom*Region, data=happy)
summary(ancovamod)

# Call:
# lm(formula = cubCorruption ~ Freedom * Region, data = happy)
#
# Residuals:
#       Min        1Q    Median        3Q       Max 
# -0.255218 -0.055910  0.002543  0.061265  0.253956 
#
# Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.40617    0.03362  12.080  < 2e-16 ***
# Freedom                          0.31375    0.09679   3.242 0.001466 ** 
# RegionAmericasCarribean          0.07311    0.09436   0.775 0.439660    
# RegionAsiaAustralia             -0.07734    0.08655  -0.894 0.372960    
# RegionEurope                    -0.18682    0.05048  -3.701 0.000301 ***
# Freedom:RegionAmericasCarribean -0.31201    0.22209  -1.405 0.162138    
# Freedom:RegionAsiaAustralia      0.09658    0.20317   0.475 0.635226    
# Freedom:RegionEurope             0.42202    0.13395   3.151 0.001969 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 0.103 on 149 degrees of freedom
# Multiple R-squared:  0.3499,	Adjusted R-squared:  0.3194 
# F-statistic: 11.46 on 7 and 149 DF,  p-value: 1.372e-11

library(interplot)
interplot(ancovamod, var1="Freedom", var2="Region")+
  labs(x="Freedom", y="coefficient for Region")

#____________________________________________________________________
# Question 6

leveneTest(Hscore ~ Region, data=happy)

cor(cancer[c("radius_mean","texture_mean","smoothness_mean")], use="na.or.complete")
#                 radius_mean texture_mean smoothness_mean
# radius_mean       1.0000000   0.32378189      0.17058119
# texture_mean      0.3237819   1.00000000     -0.02338852
# smoothness_mean   0.1705812  -0.02338852      1.00000000

vif(lm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer))
# radius_mean    texture_mean smoothness_mean 
#    1.158200        1.125114        1.037347 

canmodel = glm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(canmodel)

# Call:
# glm(formula = malignant ~ radius_mean + texture_mean + smoothness_mean, 
#      family = binomial(link = "logit"), data = cancer)
#
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -2.19102  -0.19403  -0.03799   0.04025   2.92583  
#
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -42.01941    4.45943  -9.423  < 2e-16 ***
# radius_mean       1.39699    0.15403   9.069  < 2e-16 ***
# texture_mean      0.38056    0.05711   6.663 2.68e-11 ***
# smoothness_mean 144.67423   19.04687   7.596 3.06e-14 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 751.44  on 568  degrees of freedom
# Residual deviance: 187.29  on 565  degrees of freedom
# AIC: 195.29
#
# Number of Fisher Scoring iterations: 8

plot(canmodel)

canmodel2 = glm(malignant~perimeter_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(canmodel2)

# Call:
# glm(formula = malignant ~ perimeter_mean + texture_mean + smoothness_mean, 
#      family = binomial(link = "logit"), data = cancer)
#
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -2.16715  -0.17908  -0.03790   0.03779   3.00637  
#
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -40.40324    4.36615  -9.254  < 2e-16 ***
# perimeter_mean    0.21008    0.02336   8.993  < 2e-16 ***
# texture_mean      0.37371    0.05779   6.466 1.01e-10 ***
# smoothness_mean 134.02497   18.73540   7.154 8.46e-13 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 751.44  on 568  degrees of freedom
# Residual deviance: 181.68  on 565  degrees of freedom
# AIC: 189.68
#
# Number of Fisher Scoring iterations: 8






