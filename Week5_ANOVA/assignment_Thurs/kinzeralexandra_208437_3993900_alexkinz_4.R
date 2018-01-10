###Exercise 1
##Extract the effect size of each factor (i.e. Treatment and Type) with $. Make sure that you comment on the results.
data(CO2)
head(CO2)
str(CO2)

CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)
boxplot(uptake~Treatment + Type, data=CO2, xlab="treatment", ylab="decrease mean (effect size)")
##Because both Treatment and Type have p-values lower than .05, we know that they both factors have significant effects on the uptake.

model.tables(CO2.aov, "mean", se=TRUE)$tables$Treatment ##shows the mean for the Treatment variable (non-chilled and chilled)
model.tables(CO2.aov, "mean", se=TRUE)$tables$Type ##shows the mean for the Type variable (Quebec or Mississippi)
#grand mean is 27.2131
model.tables(CO2.aov, "effects", se=TRUE)$tables$Treatment ##shows the effects (distance from the grand mean) for the Treatment variable (non-chilled and chilled)
model.tables(CO2.aov, "effects", se=TRUE)$tables$Type ##shows the effects (distance from the grand mean) for the Type variable (Quebec or Mississippi)

##Because the effects size is greater for Type than for Treatment, Type has a greater effect on the uptake than Treatment.

###Exercise 2
##Can you do a two-way ANOVA with lm() function and summarize and interpret the output table? This has something to do with interaction and the fact the those “Estimates” are actually regression coefficients
lmCO2 = lm(uptake~Treatment + Type + Treatment*Type, data=CO2)
summary(lmCO2)
##The summary shows the treatment/type combinations relation to the intercept (Quebec unchilled--the first combination listed).
##There is a significant difference between Mississippi chilled and the intercept.
##There is not a significant interaction between Treatment and Type, though the "." indicates that it is approaching significance.
