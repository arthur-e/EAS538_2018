#EXERCISE 1

CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)
model.tables(CO2.aov, "mean", se=TRUE) #grand mean = 27.2131
A = model.tables(CO2.aov, "effects", se=TRUE)
A$Treatment
A$Type
#Treatment: The effect size of the nonchilled treatment on uptake is 3.43 units greater than the grand mean.
#Treatment: The effect size of the chilled treatment on uptake is 3.43 below the grand mean.
#Type: The effect size of the Quebec type on uptake is 6.33 units greater than the grand mean.
#Type: The effect size of the Mississippi type on treatment is 6.33 units below the grand mean.

#EXERCISE 2

lm1 = lm(uptake~Treatment-1, data=CO2)
summary(lm1)
lm2 = lm(uptake~Type-1, data=CO2)
summary(lm2)

#The estimates of both type and treatment make sense because they equal the effect size plus the grand mean.
#Treatment estimates: nonchilled=30.643, chilled=23.783 (these are the 3.43 effect size above and below the grand mean of 27.2131)
#Type estimates: Quebec=33.543, Mississippi=20.883 (these are the 6.33 effect size above and below the grand mean of 27.2131)

lm3 = lm(uptake~Treatment+Type-1, data=CO2)
summary(lm3)

#Uptake = 36.973(nonchilled) + 30.113(chilled) + -12.66(Mississippi) + 0(Quebec)
