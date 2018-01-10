## Assignment #4 
## Christopher Askew-Merwin
## 2/5/2017

## Exercise 1

data(CO2)

CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)

model.tables(CO2.aov, "effects", se=TRUE)$tables$Treatment
model.tables(CO2.aov, "effects", se=TRUE)$tables$Type

## These "effects" are the differences of each treatment's means from the grand mean

## Exercise 2

## ??????????