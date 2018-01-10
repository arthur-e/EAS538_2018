data(CO2)
head(CO2, 10)
str(CO2)

CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)

#Exercise 1
?model.tables
CO2.aov.Treatment = aov(uptake~Treatment, data = CO2)
summary(CO2.aov.Treatment)
model.tables(CO2.aov.Treatment, "effects", se=TRUE)
#Tables of effects

#Treatment 
#Treatment
#nonchilled    chilled 
#3.43      -3.43 

#Standard errors of effects
#Treatment
#1.591
#replic.        42

model.tables(CO2.aov.Treatment, "means", se=TRUE)

CO2.aov.Type = aov(uptake~Type, data = CO2)
summary(CO2.aov.Type)
model.tables(CO2.aov.Type, "effects", se=TRUE)
#Tables of effects

#Type 
#Type
#Quebec Mississippi 
#6.33       -6.33 

#Standard errors of effects
#Type
#1.357
#replic.    42
model.tables(CO2.aov.Type, "means", se=TRUE)
mean(CO2$uptake)

# The results for both the factors (Type and treatment) show by how much the total mean of the observation would differ for different levels within each factor.
# I was able to intercept that my studying the the model.table function and comparing how different type(effects, means) argument results


#Exercise 2

CO2.aov.lm = lm(uptake~Treatment+Type, data = CO2)
summary(CO2.aov.lm)
#The linear model shows the estimate of each treatment with the difference of the first treatment.
#In this case the the estimates are way lower than the intercept and the p-values are less than 0.05 thus rejecting our null hypothesis that the variances for both the treatments are same.
