####EXERCISE 1:
#Extract the effect size of each factor (i.e. Treatment and Type) with $. 
#Make sure that you comment on the results.

model.tables(CO2.aov, "mean", se=TRUE)$tables
model.tables(CO2.aov, "effects", se=TRUE)$tables

#The effect size of each treatment is the difference between the mean of 
#each treatment and the Grand mean, which is 27.2131:

#Treatment
#nonchilled    chilled 
#3.429762  -3.429762 

#$Type
#Type
#Quebec Mississippi 
#6.329762   -6.329762 


##Exercise 2:
#I've showed you one of multiple ways to do this two way ANOVA. 
#Can you do it with lm() function and summarize (and interpret) it?

model.tables(CO2.aov, "mean", se=TRUE)$tables
CO2.lm = lm(uptake~Treatment + Type, data = CO2)
summary(CO2.lm)
CO2.lm2 = lm(uptake~Treatment + Type-1, data = CO2)
summary(CO2.lm2)

#$Treatment
#Treatment
#nonchilled    chilled 
#30.64286   23.78333 

#$Type
#Type
#Quebec Mississippi 
#33.54286    20.88333 

#Based on both the lm and lm -1 tests, all treatments are significantly 
#different from each other. In the lm test, the estimates for chilled and unchilled 
#are derived from the mean of Quebec plus the effect size of cihlled and unchilled.
#The estimate for Mississipi is simply the difference between its mean and the 
#mean for Quebec
