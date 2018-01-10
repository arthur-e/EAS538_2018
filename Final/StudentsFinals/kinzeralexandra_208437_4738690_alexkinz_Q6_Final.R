#6)	Which factors are significantly associated with whether a breast 
#cancer tumor is malignant or not? Choose three continuous independent
#variables to include in your model. cancer

cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

#categorical dep
#multiple continuous ind

#Check for correlation
cor(cancer)

#glm with binomial distribution

cancerMOD2 = glm(malignant~perimeter_mean + texture_mean + smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(cancerMOD2)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-2.16715  -0.17908  -0.03790   0.03779   3.00637  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -40.40324    4.36615  -9.254  < 2e-16 ***
#  perimeter_mean    0.21008    0.02336   8.993  < 2e-16 ***
#  texture_mean      0.37371    0.05779   6.466 1.01e-10 ***
#  smoothness_mean 134.02497   18.73540   7.154 8.46e-13 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 751.44  on 568  degrees of freedom
#Residual deviance: 181.68  on 565  degrees of freedom
#AIC: 189.68

#Number of Fisher Scoring iterations: 8




##OLD OUTPUT FOR area, perimeter, and radius
#Call:
#  glm(formula = malignant ~ radius_mean + area_mean + perimeter_mean, 
#      family = binomial(link = "logit"), data = cancer)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-3.11112  -0.32517  -0.16236   0.01829   2.80074  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     7.10412    6.03037   1.178  0.23877    
#radius_mean    -9.07843    1.42769  -6.359 2.03e-10 ***
#  area_mean       0.03376    0.01065   3.170  0.00152 ** 
#  perimeter_mean  1.08267    0.14640   7.396 1.41e-13 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 751.44  on 568  degrees of freedom
#Residual deviance: 240.94  on 565  degrees of freedom
#AIC: 248.94

#Number of Fisher Scoring iterations: 8