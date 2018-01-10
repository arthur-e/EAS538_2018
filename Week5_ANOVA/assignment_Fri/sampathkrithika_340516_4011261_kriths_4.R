data("CO2")
CO2
#treatments: chilled, non chilled
#uptake is dependent on plant individual, its origin (qubec or mississipi), treatment and ambient concentration

### Exercise 1
# the model tables gve an idea of how each factor affects the overall mean. A positive effect implies an increase in the mean and vice versa. 
#Effect size of type:
CO2.aov1=aov(uptake~Type, data=CO2)
summary(CO2.aov1)
model.tables(CO2.aov1, "effects", se= TRUE)
# effect of limiting plant type to Qubec increases the overall uptake mean by 6.33, while limiting it to Mississippi reduces the uptake mean by 6.33

#Effect size of Treatment:
CO2.aov2=aov(uptake~Treatment, data=CO2)
summary(CO2.aov2)
model.tables(CO2.aov2, "effects", se= TRUE)
#non chilled treatment increases the uptake mean by 3.43 while chilled reduces the uptake mean by 3.43

#Effect size of Plant:
CO2.aov3=aov(uptake~Plant, data=CO2)
summary(CO2.aov3)
model.tables(CO2.aov3, "effects", se= TRUE)
# Qn1, 2, 3 plant individuals have varying positive effects on the uptake mean, while the Mn1,2,3 plant individuals have varying negative effect on the uptake mean

#Effect size of ambient concentration:
CO2.aov4=aov(uptake~conc, data=CO2)
summary(CO2.aov4)
model.tables(CO2.aov4, "effects", se= TRUE)
# non numeric nature of the factor (ambient concentration) results in an error in computing the model tables

### EXERCISE 2
#Effect size of Treatment:
CO2.lm = lm(uptake~Treatment + Type, data = CO2)
summary(CO2.lm)
model.tables(CO2.lm, "effects", se=TRUE)
#note: despite doing multiple iterations and variations of the above code (and copy pasting from rpubs.com) , I still got the error in UseMethod message. Could you help me out?
