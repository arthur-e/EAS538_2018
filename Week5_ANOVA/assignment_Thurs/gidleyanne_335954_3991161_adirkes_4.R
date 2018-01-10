## Week 5 Lab
# Anne Gidley
# adirkes

## Exercise 1
data(CO2)
boxplot(uptake~Treatment*Type, data=CO2,col=(c("lightgreen","lightblue")))
CO2.aov = aov(uptake~Treatment+Type, data=CO2)
# summary(CO2.aov)
# results: significant differences for type and treatment
# F-stat (treatment) = 14.95
# F-stat (type) = 50.92

Type.effect = model.tables(CO2.aov, "effects",se=TRUE)$tables$Type
Type.effect
# there is a significant effect of type on CO2 uptake
# the fact that the effect for Quebec = -effect for Missisippi shows
# that each type has the same magnitude of effect, but opposite results
Treat.effect = model.tables(CO2.aov, "effects",se=TRUE)$tables$Treatment
Treat.effect
# there is a significant effect of treatment on CO2 uptake
# the fact that the effect for chilled = - effect for nonchilled shows
# that each type affects the plants in opposite ways, but with equal magnitudes


## Exercise 2
CO2.aov.2 = lm(uptake~Treatment+Type, data=CO2)
summary(CO2.aov.2)
# results show that there are significant effects of Type and Treatment on uptake
# p < 0.001
# The linear model fits the data when both treatment and type affect uptake.