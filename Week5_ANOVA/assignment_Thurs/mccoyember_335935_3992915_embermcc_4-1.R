data(CO2)
head(CO2, 10)
str(CO2)

#Exercise 1 extract the effect size of each factor (treatment and type) with $

CO2.aov=aov(uptake~Treatment + Type, data = CO2)

str(model.tables(CO2.aov, "effects", se=TRUE))

model.tables(CO2.aov, "mean", se=TRUE) # Grand mean is 27.2131 units

A = model.tables(CO2.aov, "effects", se=TRUE)$tables
A$Treatment
A$Type
# This r code gives us the effect of treatment and type on uptake compared to the grand mean.
# For the effect size of treatment, the effect of nonchilled treatment on uptake is 3.43 units greater than the grand mean (27.2131) so 30.643 and the effect of chilled treatment on uptake is 3.43 units less than the grand mean (27.2131) so 23.783. They are both equally different from the mean.
# For the effect size of type, the Quebec type on uptake is 6.33 greater than the grand mean (27.2131) so 33.54, and the effect of Mississippi type on uptake is 6.33 less than the grand mean (27.2131) so 20.88. They are both equally different from the mean.


#Exercise 2 

CO2.lm3 = lm(uptake~ Treatment + Type + Treatment * Type -1, data=CO2)
summary(CO2.lm3)

#35.333 is the mean uptake for nonchilled Quebec plants
#31.752 is the mean uptake for chilled Quebec plants
#For nonchilled Mississippi plants, the mean decreaeses by 9.381
#For chilled Mississippi plants, the mean decreases by 6.557

