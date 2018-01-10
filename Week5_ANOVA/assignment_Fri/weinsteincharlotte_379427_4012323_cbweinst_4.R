#### EXERCISE 1

data(CO2)

#run ANOVA test
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
#view ANOVA results
summary(CO2.aov)
#make effects table using ANOVA results
table = model.tables(CO2.aov, "effects", se=TRUE)
#view effects table to identify values
str(table)
#extract treatment effect sizes
treatment_effects = table$tables$Treatment
print(treatment_effects)
#extract type effect sizes
type_effects = table$tables$Type
print(type_effects)

#Treatment effect sizes:
#nonchilled = 3.429762
#chilled = -3.429762
#These values are the differences of the mean of each treatment category from the 
#overall mean (grand mean). i.e., the mean uptake of nonchilled is 3.429762 greater than the
#grand mean, and mean uptake of chilled is 3.429762 below the grand mean.

#Type effect sizes:
#Quebec = 6.329762
#Mississippi = -6.329762
#These values are the differences of the mean of each type category from the 
#overall mean (grand mean). i.e., the mean uptake of Quebec is 6.329672 greater than the 
#grand mean, and mean uptake of Mississippi is 6.329762 below the grand mean.




#### EXERCISE 2

CO2.lm = lm(uptake~Treatment*Type, data=CO2)
summary(CO2.lm)
#The output here shows a comparison between uptakes for the first category (unchilled treatment and
#type Quebec) and uptakes for the second category (chilled treatment and type Mississippi).
#This lm calculation includes an interaction term, indicating that the two changes in category
#can have an effect on one another.