# Exercise1
data(CO2)
head(CO2, 10)
str(CO2)

# uptake factor 
boxplot(uptake~Treatment*Type, data = CO2, xlab="treatment", ylab="uptake")
CO2.aov.1 = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov.1)
x=model.tables(CO2.aov.1, "effects", se=TRUE)
x$tables$Treatment
# regardless of type, the nonchilled treatment will result 3.43 above the grant mean in uptake rate
# regardless of type, the chilled treatment will result 3.43 below the grant mean in uptake rate
x$tables$Type
# regardless of treatment, Quebec will result 6.33 above the grant mean in uptake rate
# regardless of treatment, Mississippi will result 6.33 below the grant mean in uptake rate

#Exercise2 
co2.aov.2=lm(uptake~Treatment, data=CO2)
summary(co2.aov.2)
# both p value are less than 0.05 which means both treatments are significant to uptake

co2.aov.3=lm(uptake~Type, data=CO2)
summary(co2.aov.3)
# both p value are less than 0.05 which means both types are significant to uptake

co2.aov.4=lm(uptake~Treatment*Type, data=CO2)
summary(co2.aov.4)
# the p value for the nonchilled Quebec is less than 0.05 which means the interaction is significant to uptake
# the p value for the chilled Quebec is greater than 0.05 which means the interaction is not significant to uptake
# the p value for the nonchilled Mississippi is less than 0.05 whih means the interaction is significant to uptake 
# the p value for the chilled Mississippi is greater than 0.05 which means the interaction is not significant to uptake

 