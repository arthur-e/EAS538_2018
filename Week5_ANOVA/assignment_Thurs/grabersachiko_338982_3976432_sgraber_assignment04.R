#Assignment 4 02 Feb
setwd("~/Documents/MICHIGAN/538 stats/Lab 5")
library(ggplot2)

data(CO2)

####Exercise 1
CO2.aov = aov(uptake~Treatment + Type,data=CO2)
Effects=model.tables(CO2.aov,"effects",se=TRUE)
Effects$tables$Treatment
Effects$tables$Type
#These effects represent the expected change from the grand mean that occurs when a sample comes from each of the treatment groups
#For instance, a nonchilled sample will have an uptake of 3.4 greater than an average sample, with mean uptake of 27.2.
#The effects of Type are greater than the effects of Treatment (6.3 > 3.4)

####Exercise 2
ggplot(aes(y=uptake,x=Treatment,fill=Type),data=CO2) + geom_boxplot()
CO2.aov.lm = lm(uptake~Treatment + Type , data=CO2)
summary(CO2.aov.lm)

#Compared to a baseline of samples from the (nonchilled and Quebec) group,
#a change to a chilled sample decreases uptake by 6.8
#a change to Mississippi decreases uptake by 12.6
#both of these are significant changes with p<0.05
#this assessment ignores interaction terms and assumes linearity

#a better model, although still one constrained by linearity, would include the interaction term:
CO2.aov.lm2 = lm(uptake~Treatment + Type + Treatment*Type , data=CO2)
summary(CO2.aov.lm2)
#Here, we calculate that compared to the same baseline (nonchilled + Quebec), with mean uptake 35.3:
##a change to a chilled sample decreases mean uptake by 3.6
##a change to Mississippi decreases uptake by 9.38
##a change to a chilled sample from Mississippi involves all 3 coefficients:
###uptake is decreased by (3.5 + 9.4 + 6.6 = 19.5)