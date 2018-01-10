### Exercise 1-----------------------------------------------------------------------------

data(CO2)
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
model.tables(CO2.aov, "mean", se=TRUE)

###Tables of means
                  Grand mean

                             27.2131 

                  Treatment 
                  Treatment
                    nonchilled    chilled 
                       30.643     23.783 

                  Type 
                  Type
                    Quebec    Mississippi 
                     33.54       20.88 
### Effect size = (the means of difrent treatment or type) - (Garnd mean)
                     
A=model.tables(CO2.aov, "effects", se=TRUE)$tables
A$Treatment
                 Treatment
                 nonchilled    chilled 
                    3.429762  -3.429762
A$Type                    
                 Type
                  Quebec    Mississippi 
                  6.329762   -6.329762   
 ### we use R get the same result of effect size. Effect size is useing Grand mean as a baisc line which could indicate whether the mean of diffrent variables is over (positive) or lower (negtive) than the grand mean.


###Exrcise 2-------------------------------------------------------------------------------------
                  
CO2.aov1 = lm(uptake~Treatment*Type, data=CO2)
summary(CO2.aov1)


               ###Residuals:
                 Min      1Q  Median      3Q     Max 
               -22.452  -3.624   2.167   5.773  10.648 

                 Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
                 (Intercept)                        35.333      1.747  20.225  < 2e-16 ***
                 Treatmentchilled                   -3.581      2.471  -1.449 0.151141    
                 TypeMississippi                    -9.381      2.471  -3.797 0.000284 ***
                 Treatmentchilled:TypeMississippi   -6.557      3.494  -1.877 0.064213 .  

### the first line--Intercept, it starts with the point which represent the mean of uptake of QuebecNonchilled. 
### the second Treatmentchilled means the decrease when we change from QuebecNonchille to QuebecChill, the p-value>0.05 which indicate within the tyep of Quebec the change of treatment do not have sinificance difference.
### the third TypeMississippi means the diffreces when we change from QuebecNonchill to MissipianNonchill, the p-value<0.05 which indicate within the same nonchill treatment the change of type do cause significant diffrences.
### the fourth Treatmentchilled:TypeMississippi actualy represent the estimate (regression coefficient) which describes how much more uptake would decrease from nonchilled to chilled in Mississippi than in Quebec. The p-value>0.05 which indicate their degree of decrese is similiar to each other, but it very close to 0.05, which means the degree of variance close to significant.          
### but the lm() test can not tell us which factor's influce is significant in totoal, we still need to run anaova to figure it out. 
