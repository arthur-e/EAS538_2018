data("OrchardSprays")
head(OrchardSprays)
pairs(OrchardSprays, main = "OrchardSprays data")
str(OrchardSprays)
boxplot(decrease~treatment, data=OrchardSprays, xlab="treatment", ylab="decrease mean (effect size)")

#Performing Anova
spray.aov1 = aov(decrease~treatment, data=OrchardSprays)
summary(spray.aov1)


spray.aov2.1=lm(decrease~treatment, data=OrchardSprays)
summary(spray.aov2.1)


spray.aov2.2 = lm(decrease~treatment-1, data=OrchardSprays)
summary(spray.aov2.2)

OrchardSprays$treatment = factor(OrchardSprays$treatment, levels=(c("B", "A", "C", "D", "E", "F", "G", "H")))
spray.aov2.3 = lm(decrease~treatment, data=OrchardSprays)
summary(spray.aov2.3)

#Post-hoc Test
TukeyHSD(spray.aov1)

#ANOVA for factorial design
#2.1
data("CO2")
head(CO2, 10)
str(CO2)

boxplot(uptake~Treatment*Type, data=CO2, 
        xlab="treatment", ylab="decrease mean (effect size)")

CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)

#Exercise 1:
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
CO2.aov.1=lm(uptake~Treatment + Type, data = CO2)
summary(CO2.aov.1)

model.tables(CO2.aov, "effects", se=TRUE)
str(model.tables(CO2.aov, "effects", se=TRUE))

#List of 3
#$ tables:List of 2
#..$ Treatment: mtable [1:2(1d)] 3.43 -3.43
#.. ..- attr(*, "dimnames")=List of 1
#.. .. ..$ Treatment: chr [1:2] "nonchilled" "chilled"
#..$ Type     : mtable [1:2(1d)] 6.33 -6.33
#.. ..- attr(*, "dimnames")=List of 1
#.. .. ..$ Type: chr [1:2] "Quebec" "Mississippi"
#$ n     : Named int [1:2] 42 42
#..- attr(*, "names")= chr [1:2] "Treatment" "Type"
#$ se    :Class 'mtable'  atomic [1:2] 1.25 1.25
#.. ..- attr(*, "type")= chr "effects"
#- attr(*, "type")= chr "effects"
#- attr(*, "class")= chr [1:2] "tables_aov" "list.of"

model.tables(CO2.aov, "effects", se=TRUE)$tables$Treatment
#Treatment
#nonchilled    chilled 
#3.429762  -3.429762 

#Though the use of this effects tables and treatment pull out that we ran in the code above, we can compare the means between all of the values, to that of the grand mean. 
#Exercise 2
CO2.aov.2 = lm(uptake~Treatment-1 + Type, data=CO2)
summary(CO2.aov.2)

#In this Anova test, we do not have an intercept that we are comparing our results from the samples, like we do when we do not have a minus 1 as part of our treatment. This allows us to compare the means of all of the values, rather than just comparing them to an intercept. 

#Non-parametric one-way ANOVA
kruskal.test(uptake~Treatment, data = CO2)
