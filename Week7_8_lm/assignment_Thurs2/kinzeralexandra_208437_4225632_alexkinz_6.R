data("airquality")
head(airquality, 15)

#####Exercise 3
ozoneMOD = lm(Temp~Ozone, data=airquality)
summary(ozoneMOD)

##Calculate the residual standard error (RSE) and mean square error (MSE) manually for the model you built last time (Ozone as the independent variable).

#manually calculate the residual standard error (RSE)
resids = residuals(ozoneMOD) ##calculate the residuals
RSS = sum(resids^2) ##calculate the residual sum of squares
RSE = sqrt(RSS/summary(ozoneMOD)$df[2])
RSE # = 6.818914
##The RSE is the variance of all residuals. It expresses the variability of the dependent variable that is not explained by our model.

#manually calculate the mean square error (MSE)
MSE = RSS/summary(ozoneMOD)$df[2]
MSE #MSE = 46.49759
##The MSE is the standard deviation of all residuals.

#check the MSE with the anova() function
anova(ozoneMOD)
#Analysis of Variance Table

#Response: Temp
#            Df  Sum Sq   Mean Sq   F value    Pr(>F)    
#Ozone       1   5046.3   5046.3    108.53     < 2.2e-16 ***
#Residuals 114   5300.7    46.5 

#The MSE (Mean Sq) is roughly the same in the ANOVA table. 

#####EXERCISE 4
#get rid of NAs in data
airquality_nona = airquality[!(is.na(airquality$Ozone)),]
airquality_nona

#rerun model
ozoneMOD_nona = lm(Temp~Ozone, data=airquality_nona)
summary(ozoneMOD_nona)
resids_1 = residuals(ozoneMOD_nona) ##calculate the residuals
RSS_1 = sum(resids_1^2) ##calculate the residual sum of squares
RSE_1 = sqrt(RSS_1/summary(ozoneMOD_nona)$df[2])
RSE_1 # = 6.818914

#Calculate adjusted R^2 manually
SSY_1 = deviance(lm(airquality_nona$Temp~1))
R2adj = 1 - (RSS_1/114)/(SSY_1/115) 
R2adj #0.4832134

##Confirm with ANOVA
anova(ozoneMOD_nona)
summary(ozoneMOD_nona)
##The adjusted R-squared in the ANOVA summary table confirms the 0.4832 value I just calculated.


#####EXERCISE 5
#Check residual independency, homoscedasticity, and normality 
#for the model with ozone as the independent variable and interpret 
#the results briefly.

#Plot residuals to check for autocorrelation
plot(residuals(ozoneMOD))

#Run Durbin-Watson to test for autocorrelation (is ther a pattern in the deviation from the linear model?)
dwtest(ozoneMOD, alternative=c("two.sided"))
#RESULTS
#Durbin-Watson test
#data:  ozoneMOD
#DW = 1.1833, p-value = 7.276e-06
#alternative hypothesis: true autocorrelation is not 0

#Because the P-value is less than .05, we accept the null hypothesis that the autocorrelation is equal to 0.
#There is a pattern to the residuals that is not explained by the linear model.

#Check for homoscedasticity
plot(residuals(ozoneMOD)~fitted(ozoneMOD))
abline(lm(residuals(ozoneMOD)~fitted(ozoneMOD)), col="red")
bptest(ozoneMOD)
##Null hypothesis is that they are homoscedastic.
##RESULTS
##studentized Breusch-Pagan test
##data:  ozoneMOD
##BP = 3.2346, df = 1, p-value = 0.0721

#P-value above .05, we cannot reject the null, indicating that the variances are homoscedastic (the variance is constant, distributed throughout the plot, not having a pattern with th fitted line)

#Check for normality
plot(ozoneMOD, which=c(2))
shapiro.test(residuals(ozoneMOD))
#RESULTS
#Shapiro-Wilk normality test
#data:  residuals(ozoneMOD)
#W = 0.94867, p-value = 0.000226

#P-value below .05 indicates that our data is not normally distributed.
