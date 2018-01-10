#5) Choose one of the continuous independent variables that was 
#significant in the model for Question 4 and interact it with region 
#(Region) to predict corruption (Corruption). This model should only 
#include one continuous independent variable and its interaction with 
#region. Does the influence of your continuous variable on corruption 
#vary by region? If yes, how do you interpret the interaction?

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

#Ancova interaction (so with lm)

happyMOD3 = lm(Ctrans1~Freedom*Region, data=happy)
summary(happyMOD3)

#Call:
#  lm(formula = Ctrans1 ~ Freedom * Region)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.08487 -0.31735  0.07803  0.40588  1.38756 

#Coefficients:
#                                             Estimate    Std. Error    t value     Pr(>|t|)    
#(Intercept)                                  -2.6096     0.2102        -12.416     < 2e-16 ***
#Freedom                                      1.5929     0.6050        2.633       0.009360 ** 
#RegionAmericasCarribean                      0.3985     0.5898        0.676       0.500360    
#RegionAsiaAustralia                          -0.5177     0.5410        -0.957      0.340110    
#RegionEurope                                 -1.3755     0.3155        -4.360      2.42e-05 ***
#Freedom:RegionAmericasCarribean              -1.6566     1.3882        -1.193      0.234643    
#Freedom:RegionAsiaAustralia                  0.6657     1.2700        0.524       0.600949    
#Freedom:RegionEurope                         2.9826     0.8373        3.562       0.000494 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.6441 on 149 degrees of freedom
#Multiple R-squared:  0.3365,	Adjusted R-squared:  0.3053 
#F-statistic: 10.79 on 7 and 149 DF,  p-value: 5.739e-11

#Heteroscedasticity #PASSES
bptest(happyMOD3)
#studentized Breusch-Pagan test

#data:  happyMOD3
#BP = 9.0359, df = 7, p-value = 0.2501

#Normality of errors #PASSES
res2 = residuals(happyMOD3)
plot(res2) #looks good
hist(res2) #looks good, slightly skewed right
qqnorm(res2); qqline(res2, col="Red") #looks good
shapiro.test(res2) #fails with p-value of .01481

#Independence of errors #PASSES
install.packages(lmtest)
library(lmtest)
dwtest(happyMOD3, alternative=c("two.sided"))
