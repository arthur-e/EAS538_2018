setwd('D:/UM/SNRE/Winter 2017/NRE 538 Statistics/Week 5 Lab')
data("CO2")
head(CO2,10)
boxplot(uptake~Treatment*Type,data=CO2,xlab="treatment",ylab="uptake")

CO2.aov=aov(uptake~Treatment+Type,data=CO2)
summary(CO2.aov)

effsize1=model.tables(CO2.aov,type="effects",se=TRUE)$tables

effsize1$Treatment
#The CO2 uptake between treatments (chilled and nonchilled) is significantly different
#The coefficient for nonchilled grass is 3.43, and -3.43 for chilled grass
#Non-chilled grass takes up significantly more CO2 than chilled grass

effsize1$Type
#The CO2 uptake between different plant types (Quebec and Mississippi) is significantly different
#The coefficient for Quebec grass is 6.33, and -6.33 for Mississippi grass
#Quebec grass can take up significantly more CO2 than Mississippi grass

#(The effect of plant origin seems to have a bigger impact on CO2 uptake than treatment)
#(If the independent variable is continuous, the coefficient probably means how much
#the dependent variable changes with one unit of change in independent variable.
#But I'm not sure how to interpret it in more detail for categorical independent variables...)