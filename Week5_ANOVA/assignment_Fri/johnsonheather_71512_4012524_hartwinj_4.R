# setwd("C:/Users/Owner/Desktop/U of M Masters Program/Winter 2017/NRE 538/Lab/Assignments/Assignment 4")

read("https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/zCO2.html")
data(CO2)
head(CO2)
str(CO2)
head(CO2, 10)
CO2.aov2=aov(uptake~Type, data=CO2)
summary(CO2.aov2)
model.tables(CO2.aov2, "effects", se=TRUE)
CO2.aov2=aov(uptake~Treatment, data=CO2)
model.tables(CO2.aov2, "effects", se=TRUE)
summary(CO2.aov2)
CO2.aov3=aov(uptake~Treatment+Type, data=CO2)
summary(CO2.aov3)
model.tables(CO2.aov3, "effects", se=TRUE)
# Tables of effects
#Treatment 
#nonchilled    chilled 
#3.43      -3.43 
#Type
#Quebec Mississippi 
#6.33       -6.33 

#Standard errors of effects
#Treatment  Type
#1.254 1.254
#replic.        42    42

# The table of effect is showing the effect size between treatment and type and uptake.  
# For Treatment (non-chilled vs. chilled) it's showing that 3.43 is the difference between
# the grand mean of that interaction.
# On the other hand, 6.33 is the difference from the grand mean of type (Quebec vs Mississippi)


# Exercise 2
lmfinal = lm(uptake ~ Treatment + Type + Treatment * Type, data=CO2)
summary(lmfinal)

#Residuals:
#  Min      1Q  Median      3Q     Max 
# -22.452  -3.624   2.167   5.773  10.648 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                        35.333      1.747  20.225  < 2e-16 ***
#  Treatmentchilled                   -3.581      2.471  -1.449 0.151141    
# TypeMississippi                    -9.381      2.471  -3.797 0.000284 ***
#  Treatmentchilled:TypeMississippi   -6.557      3.494  -1.877 0.064213 .  

#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 8.006 on 80 degrees of freedom
# Multiple R-squared:  0.4718,	Adjusted R-squared:  0.452 
#F-statistic: 23.82 on 3 and 80 DF,  p-value: 4.106e-11

#Sorry, Oscar-I was called in to work on Tuesday so I wasn't able to finish this so
#I'm just turning in what I'd started-I know it's incomplete but I wanted to at least get something
#in before the deadline.