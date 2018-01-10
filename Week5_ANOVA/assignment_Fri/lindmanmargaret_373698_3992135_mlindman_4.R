#Ex1
data("CO2")
head(CO2)
str(CO2)
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)
(model.tables(CO2.aov, "effects", se=TRUE))
#These results show the mean (effect size) of each treatment and type (in relation to the grand mean). Results suggest that nonchilled and Quebec plants have greater CO2 uptake. However, 'Type' (Quebec vs. Mississippi) seems to have a greater effect on CO2 uptake than "Treatment."

#EX2
CO2.aov2.1 = lm(uptake~Treatment, data=CO2)
summary(CO2.aov2.1)
#Results suggest that Treatment does have a significant effect on CO2 uptake.
