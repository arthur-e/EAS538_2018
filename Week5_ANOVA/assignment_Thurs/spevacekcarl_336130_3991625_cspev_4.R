###Exercise 1
data("CO2")
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)

model.tables(CO2.aov, "mean", se=TRUE)

A = model.tables(CO2.aov, "effects", se=TRUE)$tables
A$Treatment
A$Type
###Grand Mean = 27.2131
###This gives effect on uptake, compared to the grand mean of uptake across both variables.
##The effect of non-chilled on uptake is 3.429762 units greater than the grand mean, while the effect of chilled is 3.429762 units less than the grand mean.
##The effect of Quebec is 6.329762 units greater than the grand mean, while the effect of Mississipi is 6.329762 units less than the grand mean.

###Exercise 2
lm3 = lm(uptake~Treatment * Type-1, data = CO2)
summary(lm3)
##The coefficient estimates show the actual total effect of each possible plant and treatment type.
##The first value is the uptake of nonchilled Quebec plants, the second value is the uptake of chilled Quebec plants. Uptake decreases by 9.381 for nonchilled Mississippi plants and by 6.557 for chilled Mississippi plants.
