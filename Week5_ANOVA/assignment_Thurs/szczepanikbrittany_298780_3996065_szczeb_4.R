data(CO2)
head(CO2, 10)
str(CO2)

#NOTE: I was trying to make sure I understood exercise 1 correctly, so I did a one-way first and then combined them at the end)

##Exercise 1##

##Null hypothesis is that the chilled and non-chilled treatments have equal uptake

Co2.aov.treatment = aov(CO2$uptake~CO2$Treatment, data = CO2)
model.tables(CO2.aov.treatment, "effects", se=TRUE)
boxplot(CO2$uptake~CO2$Treatment, data=CO2, xlab="uptake", ylab="Treatment")
summary(Co2.aov.treatment)

#The effect of chilled is 3.43, and the effect of the non-chilled is -3.43.  Essentially, having the chilled treatment increases the mean of the uptake by 3.43, whereas the effect of the nonchilled treatment decreases the mean by 3.43.
#Additionally, the F statistic compares the variation among sample means to the variation within groups.  Since our F statistic is higher (i.e. the numerator dominates, suggesting high variation among the sample means) and the p-value is less than 0.o5, we can reject the null hypothesis and accept the alternative hypothesis--that the chilled and non-chilled treatments have different uptakes.


##Null hypothesis is that Quebec and Mississippi types have equal uptake

CO2.aov.type = aov(CO2$uptake~CO2$Type, data=CO2)
model.tables(CO2.aov.type, "effects", se=TRUE)
boxplot(CO2$uptake~CO2$Type, data=CO2, xlab="uptake", ylab="Type")
summary(CO2.aov.type)

#The effect of Quebec is 6.33, and the effect of Mississippi is -6.33.  Essentially, having Quebec increases the mean of the uptake by 6.33, whereas the effect of Mississippi decreases the mean by 6.33.
# Additionally, similar to the previous example, the F statistic is higher (i.e. the numerator dominates, suggesting high variation among the sample means) and the p-value is less than 0.o5, we can reject the null hypothesis and accept the alternative hypothesis--that the Quebec and Mississippi types have different uptakes.

CO2.aov = aov(CO2$uptake~CO2$Treatment+CO2$Type, data = CO2)
model.tables(CO2.aov, "effects", se=TRUE)
boxplot(CO2$uptake~CO2$Treatment+CO2$Type, data=CO2, xlab="uptake", ylab="Treatment + Type")
summary(CO2.aov)

##Doing a two-way ANOVA suggests the same thing, that all of these treatment and types have different uptakes, and we can reject the null hypothesis that there is no difference between them.



##Exercise 2##

CO2.lm.1 = lm(CO2$uptake~CO2$Treatment+CO2$Type, data = CO2)
boxplot(CO2$uptake~CO2$Treatment+CO2$Type, data=CO2, xlab="uptake", ylab="Treatment + Type")
summary(CO2.lm.1)

#Using the lm function in the same way we were in class yields an estimate of the change in uptake due ot the change in type and treatment, but they are independent of one another, so we have to alter the test to include an interaction term (shown below).  

CO2.lm.2 = lm(CO2$uptake~CO2$Treatment*CO2$Type, data = CO2)
boxplot(CO2$uptake~CO2$Treatment+CO2$Type, data=CO2, xlab="uptake", ylab="Treatment + Type")
summary(CO2.lm.2)

#Since the treatment and type are now interacting with one another, any alteration to one of those two terms changes the other one--they are now dependent upon one another, which gives us a new category of "treatmentchilled:typemississippi" which shows the change of uptake from nonchilled to chilled treatments in Quebec vs. Mississippi plants.
#Knowing this, we can see that the treatment of chilled plants in Mississippi has a lower uptake (-3.581) than the chilled treatment for Quebec plants, and that the Mississippi plants have 9.381 lower uptake than the Quebec plants in terms of nonchilled treatment.