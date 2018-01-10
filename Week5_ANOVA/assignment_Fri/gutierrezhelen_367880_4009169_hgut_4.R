setwd("C:/Users/helen/Documents/winter_2017/538_stats/lab/W5")
data(CO2)
head(CO2, 10)
str(CO2)

#EXERCISE 1:

#H0: uptake = Treatment + Type
#H1:uptake =/= Treatment + Type

CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)

#The ANOVA test tells us that there are significant differences between the 
#Treatment and CO2 uptake, and Type of plant and CO2 uptake; however the test does 
#not indicate if there is an interaction between Treatment and Type.

model.tables(CO2.aov, "effects", se=TRUE)

#The table indicates that CO2 uptake of chilled plants is less than non-chilled plants
#The table also indicates that CO2 uptake of plants from Mississippi is less than
#CO2 uptake of plants from Quebec.

TukeyHSD(CO2.aov)

#The Tukey test indicates the effect size between each of the dependent variables.
#The p-value for Treatment is 0.00022, which indicates that the difference in uptake
#between chilled and non-chilled is significant.
#The p-value for Type is 0, which indicates that the difference in uptake
#between plants from Mississippi and Quebec is significant.