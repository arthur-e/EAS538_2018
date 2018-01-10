data(CO2)
head(CO2, 10)
str(CO2)
boxplot(uptake~Treatment*Type, data=CO2, xlab="treatment", ylab="decreased mean (effect size)")
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)


## Ex1
CO2.lm2 = lm(uptake ~ Treatment*Type, data = CO2)
summary(CO2.lm2)
CO2.lm = lm(uptake~Treatment + Type, data = CO2)
summary(CO2.lm)
model.tables(CO2.aov)
model.tables(CO2.aov, type = "effects", se = FALSE, "Treatment")
model.tables(CO2.aov, type = "effects", se = FALSE, "Type")
##The effects values for the nonchilled treatment was 3.43 and the effect of the chilled
## treatment was -3.43.
##The effect of the Quebec type was 6.33 and the effect of the Mississippi type was
## -6.33.
##These values mean that the effect that each treatment had affected the mean by 3.43
##and each type affected the mean by 6.33.



