#### Exercise 1
#### Extract the effect size of each factor (i.e. Treatment and Type) with $. 
#### Make sure that you comment on the results (e.g. what do those numbers means and how do you know that?).

data(CO2)
head(CO2, 10)

str(CO2)

boxplot(uptake~Treatment*Type, data=CO2, xlab="treatment", ylab="effect size")

CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)

CO2.Effects = model.tables(CO2.aov, "effects", se=TRUE)
CO2.Effects$tables$Treatment

## $Treatment
## Treatment
## nonchilled    chilled 
## 3.429762  -3.429762 

CO2.Effects$tables$Type

## $Type
## Type
## Quebec Mississippi 
## 6.329762   -6.329762 

## The mean uptake value of the non-chilled experimental treatments was 3.43 greater than the grand mean, and chilled
## experimental treatment were 3.43 less than the grand mean. The mean uptake value for all Quebec-orginating subjects
## was 6.33 greater than the grand mean, while the same for Mississippi-originating subjects was 6.33 less than the 
## grand mean.

CO2.Means = model.tables(CO2.aov, "means", se=TRUE)
CO2.Means$tables$Treatment

## $Treatment
## Treatment
## nonchilled    chilled 
## 30.64286   23.78333 

CO2.Means$tables$Type

## $Type
## Type
## Quebec Mississippi 
## 33.54286    20.88333

## The grand mean of the uptake value of all experimental treatments is 27.21. Subsequent values show the mean of each
## variable we are studying:
## nonchilled: 30.64
## chilled: 23.78
## Quebec-origin: 33.54
## Mississippi-origin: 20.88

#############################################################################

#### Exercise 2 (Bonus!)
#### I've showed you one of multiple ways to do this two way ANOVA. Can you do it with lm() function 
#### and summarize and interpret the output table? This has something to do with interaction and the 
#### fact the those "Estimates" are actually regression coefficients.


CO2$Treatment = factor(CO2$Treatment, levels=(c("nonchilled", "chilled")))
CO2$Type = factor(CO2$Type, levels=(c("Quebec", "Mississippi")))

# with interaction term, effect size model compared to nonchilled Quebec
CO2.AOV.2 = lm(uptake~Treatment*Type, data=CO2) 
summary(CO2.AOV.2)
## Coefficients:
##                                  Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)                        35.333      1.747  20.225  < 2e-16 ***
## Treatmentchilled                   -3.581      2.471  -1.449 0.151141    
## TypeMississippi                    -9.381      2.471  -3.797 0.000284 ***
## Treatmentchilled:TypeMississippi   -6.557      3.494  -1.877 0.064213 .

## INTERPRETATION: 35.333 is the expected effect of treatment (nonchilled) and type (Quebec). 
## Subsequent values represent the expected difference between changes in treatment and/or type from initial value.


## with interaction term, mean model 
CO2.AOV.3 = lm(uptake~Treatment*Type-1, data=CO2) 
summary(CO2.AOV.3)
## Coefficients:
##                               Estimate Std.    Error  t value  Pr(>|t|)    
## Treatmentnonchilled                35.333      1.747  20.225  < 2e-16 ***
## Treatmentchilled                   31.752      1.747  18.175  < 2e-16 ***
## TypeMississippi                    -9.381      2.471  -3.797 0.000284 ***
## Treatmentchilled:TypeMississippi   -6.557      3.494  -1.877 0.064213 .  


#no interaction term between treatment and type
CO2.AOV.4 = lm(uptake~Treatment+Type, data = CO2)
summary(CO2.AOV.4)
## Coefficients:
##                  Estimate Std.  Error  t value Pr(>|t|)    
## (Intercept)        36.973      1.536  24.065  < 2e-16 ***
## Treatmentchilled   -6.860      1.774  -3.867 0.000222 ***
## TypeMississippi   -12.660      1.774  -7.136 3.68e-10 ***












