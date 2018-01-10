# Assignment 8
# Christopher Askew-Merwin
# 3/16/2017

# Exercise 1

# Try to use the following data set to execute ANCOVA and 
# make interaction plots This data set contains the fruit 
# weight (Fruit) of different plants with different 
# root depth (root) and different treatment (Grazing).

library(RCurl)
library(Lahman)
library(dplyr)
library(ggplot2)
library(interplot)

gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)

mod = lm(Fruit~Root*Grazing, data=gz)
summary(mod)

# Call:
# lm(formula = Fruit ~ Root * Grazing, data = gz)
#
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -17.3177  -2.8320   0.1247   3.8511  17.1313 
#
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -125.173     12.811  -9.771 1.15e-11 ***
# Root                   23.240      1.531  15.182  < 2e-16 ***
# GrazingUngrazed        30.806     16.842   1.829   0.0757 .  
# Root:GrazingUngrazed    0.756      2.354   0.321   0.7500    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 6.831 on 36 degrees of freedom
# Multiple R-squared:  0.9293,	Adjusted R-squared:  0.9234 
# F-statistic: 157.6 on 3 and 36 DF,  p-value: < 2.2e-16

anova(mod)

# Analysis of Variance Table
#
# Response: Fruit
#              Df  Sum Sq Mean Sq  F value    Pr(>F)    
# Root          1 16795.0 16795.0 359.9681 < 2.2e-16 ***
# Grazing       1  5264.4  5264.4 112.8316 1.209e-12 ***
# Root:Grazing  1     4.8     4.8   0.1031      0.75    
# Residuals    36  1679.6    46.7                       
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing Treatment", y="Estimated coefficient for Root Depth", title="Estimated Coefficient of Root Depth on Grazing Treatment")

# This plot shows that the effect of Root Depth is slightly higher on Ungrazed plants
# over Grazed plants but the difference is not significant.

interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root Depth", y="Estimated coefficient for Grazing Treatment", title="Estimated Coefficient of Grazing Treatment on Root Depth")

# This plot shows that even though the coefficient for Grazing Treatment increases as
# Root Depth increases, the change is still not significant. 




