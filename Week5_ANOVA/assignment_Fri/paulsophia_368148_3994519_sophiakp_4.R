##Excercise 1
data(CO2)
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)
CO2effects=model.tables(CO2.aov, "effects", se=TRUE)
CO2effects$tables
##the differenerce is the difference between the two means, it sets the midpoint at zero
##the difference between Quebec and Mississipi is greater than the difference between chilled and nonchilled
CO2effects$se
##This is the standard errors of the type and treatment
TukeyHSD(CO2.aov)