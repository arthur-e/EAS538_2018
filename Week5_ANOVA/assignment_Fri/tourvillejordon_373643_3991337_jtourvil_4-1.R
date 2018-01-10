###Exercise 1
setwd("~/Desktop/Stats R/Stats")
data("CO2")
head(CO2, 10)
str(CO2)
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)
boxplot(uptake~Type + Treatment, data=CO2, xlab="Treatment", ylab="Uptake")
(model.tables(CO2.aov, "effects", se=TRUE))#Results give effect size for both Treatment and Type. Results and meaning below:
#Tables of effects:
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
#Values indicate differences in the means (for both type and treatment) 
#from the overall or grand mean of each variable. So, a negative value means that that particular mean
#is smaller than the grand mean, and the opposite for a positive value mean. The larger the value
#returned in this function, the greater the effect size. In this case, the absolute difference between
#chilled and non-chilled in smaller than the location differences (effect size), so type in this case
#has a greater effect on the observed result than treatment.
###
###Exercise 2 (Bonus)
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
CO2.aov2.1 = lm(uptake~Treatment + Type, data=CO2)
CO2.aov2.2 = lm(uptake~Treatment-1 + Type, data=CO2)
summary(CO2.aov)
summary(CO2.aov2.1)
summary(CO2.aov2.2)
#Returns output tables
#CO2.aov suggests that at least one group is different than the others.
#CO2.aov2.1 suggests that all treatment/type combonations are different than the first one tested.
#CO2.aov2.2 suggests that type has a greater effect size than treatment (the (lm-1) test is very important to answer this problem).
