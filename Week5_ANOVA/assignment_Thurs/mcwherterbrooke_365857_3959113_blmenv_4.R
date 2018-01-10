#Exercise 1
data(CO2)
head(CO2,10)
str(CO2)

CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)
model.tables(CO2.aov,"effects", se=TRUE)$tables
str(model.tables(CO2.aov,"effects", se=TRUE))
a=model.tables(CO2.aov,"effects", se=TRUE)$tables
a$Treatment #Effects

# The effect of the chilled and unchilled are the same distance of the Grand Mean, which would indicate a normal distribution
#$nonchilled    chilled 
# 3.429762  -3.429762 

a$Type #Effects

#The effect of Quebec and Mississippi are the same distance from the grand meaning indicating a normal distribution
# Quebec Mississippi 
# 6.329762   -6.329762 


#Exercise 2 

CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)


boxplot(uptake~Treatment + Type, data=CO2, xlab="Treatment", ylab="decrease mean (effect size)")
# F-statistic =9.282 and the p-value=0.003096
#The F statistic is greater than 1 indicating that the variation among the group means is greater than would be expected by chance and
#The p-value is less than 0.05 indicating that the results are significantly different and you can reject the null hypothesis

CO2.aov.1 = lm(uptake~Treatment+ Type-1, data=CO2)
summary(CO2.aov.1)
 #                    Estimate Std. Error t value Pr(>|t|)    
#Treatmentnonchilled   36.973      1.536  24.065  < 2e-16 ***
#Treatmentchilled      30.113      1.536  19.600  < 2e-16 *** # compared to nonchilled
#TypeMississippi      -12.660      1.774  -7.136 3.68e-10 *** #compared to type Quebec
#
#The treatment mississippi was signifcantly less than Quebec and the treatment chilled is significantly smaller than non chilled.


