#Erin Barton
#NRE538_Statistics
#2.7.2016

#Stats Lab 4


data(CO2)
head(CO2, 10)
str(CO2)

boxplot(uptake~Type*Treatment, data=CO2, xlab="treatment",ylab="uptake", col=c("gold","dark green"))

CO2.aov <- aov(uptake~Treatment+Type, data=CO2)
summary(CO2.aov)

#Exercise 1: 
#use model.tables() on anova object of two way anove example to extract the effect size of chill/nonchill, Quebec/Mississippi <-  purpose is to become familiar with using model.table and $ 
model.tables(CO2.aov, "effects" ,se=TRUE)
model.tables(CO2.aov$Treatment, "effects", se=TRUE)

CO2_tables <- model.tables(CO2.aov,"effects",se=TRUE)
CO2_tables

str(CO2_tables)

treatment_effectsize <- CO2_tables$tables$Treatment
treatment_effectsize
#Treatment
#nonchilled    chilled 
#3.429762  -3.429762 


type_effectsize <- CO2_tables$table$Type
type_effectsize
#Type
#Quebec Mississippi 
#6.329762   -6.329762

#In each of these cases we can see that the effects What this means is that the difference between the mean of all effects (the grand mean) and 
#the mean of the nonchilled treatment is 3.43, and the difference between the grand mean and 
#the chilled treatment is -3.43. So we can see that the two treatment types both have an effect and what the effects actually are. 
#The ANOVA test tells us that there is a significant difference between groups, and the by looking at the effect sizes in isolation we can see what that difference is. 



