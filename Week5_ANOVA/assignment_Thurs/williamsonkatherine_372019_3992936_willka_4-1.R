#Exercise 1
data("CO2")
head(CO2, 10)
boxplot(uptake~Treatment*Type, data=CO2, xlab="treatment", ylab="decrease mean (effect size)")
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov) #Treatment and Type are significant at .001 alpha level (p-value)
CO2.aov1 = aov(uptake~Treatment*Type, data = CO2)
summary(CO2.aov1) # interaction of Treatment and Type are significant at .1 alpha level (p-value)
TukeyHSD(CO2.aov)
TukeyHSD(CO2.aov1) # confirmation of differences, where Quebec and non-chilled variables increase uptake rate.

str(model.tables(CO2.aov1, "effects", se=TRUE))
mean(CO2$uptake) # grand mean = 27.21
A=model.tables(CO2.aov, "effects", se=TRUE)$tables
B=model.tables(CO2.aov, "means", se=TRUE)$tables
A$Treatment # +/-3.43 difference from the grand mean for each Treatment mean, 23.78 (chilled) and 30.64 (nonchilled), so nonchilled treatments created larger average CO2 uptake.
A$Type # +/- 6.33 difference from the grand mean for each Type mean, 20.88 (Mississippi) and 33.54 (Quebec), so Quebec plants have higher average CO2 uptake
# Using the boxplot, these table results, and the results of the ANOVA and Tukey tests for significance above, we see that the origins of the plant (Type) as well as between the kind of treatment each significantly affect CO2 uptake rates and explain the variation between the data. We can also see with the F values that the Type has a bigger impact than the Treatment even though both contribute to differences in variation between samples. The F statistics for both are large and highly unlikely assuming the populations means are equal (null hypothesis). Thus we would reject the null hypothesis in this case.

#Exercise 2
lm.data=lm(uptake~Type+Treatment-1, data=CO2)
summary(lm.data)
# Difference between TypeMississippi and TypeQuebec is 12.66, which is 6.33x2, or the total range of difference around the mean for Type. This also shows Quebec's higher CO2 uptake rate relative to Mississippi.
# Treatmentchilled is -6.86, which is 3.43x2, or the range of difference around the mean for Treatment. 
lm.data2=lm(uptake~Type*Treatment-1, data=CO2)
summary(lm.data2)
# not sure why there are missing combinations in this output table, but shows that there is an interaction effect at the .1 alpha level.