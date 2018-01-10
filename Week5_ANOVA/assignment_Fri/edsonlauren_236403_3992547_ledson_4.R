##Lauren Edson 
##Lab 4 ANOVA

data(CO2)
head(CO2)
# Plant   Type  Treatment conc uptake
# 1    Qn1 Quebec nonchilled   95   16.0
# 2    Qn1 Quebec nonchilled  175   30.4
# 3    Qn1 Quebec nonchilled  250   34.8
# 4    Qn1 Quebec nonchilled  350   37.2
# 5    Qn1 Quebec nonchilled  500   35.3
# 6    Qn1 Quebec nonchilled  675   39.2

boxplot(uptake~Treatment*Type, data=CO2, xlab="treatment", ylab="decrease mean (effect size)")

#one way anova
CO2.aov.treatment = aov(uptake~Treatment, data=CO2)
summary(CO2.aov.treatment)

#one way anova
CO2.aov.type = aov(uptake~Type, data=CO2)
summary(CO2.aov.type)

#two way anova
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)

####

###Exercise 1
##Extract the effect size of each factor (i.e. Treatment and Type) with $. 
##Make sure that you comment on the results.

str(model.tables(CO2.aov, "effects", se=TRUE))
A = model.tables(CO2.aov, "effects", se=TRUE)$tables
A$Treatment
# Treatment
# nonchilled    chilled 
# 3.429762  -3.429762 

##This tells us that the two treatments are evenly spaced around the grand mean. 
## The difference from the mean of the "treatment" is +/- 3.43 in each direction.
##Since it is evenly spaced, we can interprete this to mean that the distribution is normal.

A$Type
# Type
# Quebec Mississippi 
# 6.329762   -6.329762

##This tells us that the two treatments are evenly spaced around the grand mean. 
## The difference from the mean of the "type" is +/- 6.32 in each direction.
##Since it is evenly spaced, we can interprete this to mean that the distribution is normal.


####

###Exercise 2
##I've shown you one of multiple ways to do this two way ANOVA. 
##Can you do it with lm() function and summarize (and interpret) it?

CO2.lm = lm(uptake~Treatment + Type + Treatment*Type, data=CO2)
summary.aov(CO2.lm)
#                   Df Sum Sq Mean Sq F value   Pr(>F)    
#   Treatment       1    988     988  15.416 0.000182 ***
#   Type            1   3366    3366  52.509 2.38e-10 ***
#   Treatment:Type  1    226     226   3.522 0.064213 .  
#   Residuals      80   5128      64                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


##The summary of these results shows us that Treatment and Type are very significant. 
## And, Treatment*Type is somewhat significant (0.1). To interpret these results, 
## we can think of "non-chilled Quebec" as the control to which we compare the other
## treatment, type combinations. "Chilled, Mississippi" is different than "non-chilled Quebec
## but, we cannot tell if it is the Treatment or the Type.