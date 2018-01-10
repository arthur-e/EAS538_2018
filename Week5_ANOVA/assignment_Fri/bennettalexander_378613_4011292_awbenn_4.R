## set working directory
setwd("/docs/umich/coursework/538/labs/lab05")

## import data
data("CO2")

## exercise 1
## perform ANOVA by treatment and plant type
CO2.aov = aov(uptake~Treatment + Type, data=CO2)

## generate summary tables of effects for the ANOVA variables
CO2.model = model.tables(CO2.aov, "effects", se=TRUE)

## extract effects by treatment and type variables
Treat.eff = CO2.model$tables$Treatment
Type.eff = CO2.model$tables$Type
## these values represent the relative increase or decrease 
## of the variable subsets vs. the overall means

## this can be demonstrated by calculating differences from overall mean uptake
Q.sub = subset(CO2,Type=="Quebec")
mean(Q.sub$uptake) - mean(CO2$uptake)
## the result matches the value given in the model table

## The Tukey test shows that both effects are significant
TukeyHSD(CO2.aov)

## exercise 2
## perform linear model ANOVA and summarize
CO2.lm = lm(uptake~Treatment + Type-1, data=CO2)
summary(CO2.lm)
## the regression coefficients displayed show a relative positive relationship by treatment,
## with a decreasing effect in the chilled cohort, and a negative relationship by type