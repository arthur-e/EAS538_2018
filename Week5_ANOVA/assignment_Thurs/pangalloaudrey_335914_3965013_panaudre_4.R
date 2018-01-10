data("OrchardSprays")
head(OrchardSprays)
pairs(OrchardSprays, main= "OrchardSprays data")
str(OrchardSprays)
boxplot(decrease~treatment, data=OrchardSprays, xlab="treatment", ylab="decrease mean (effect size)")
spray.aov1= aov(decrease~treatment, data=OrchardSprays)
spray.aov2.1 = lm(decrease~treatment, data= OrchardSprays)
spray.aov2.2= lm(decrease~treatment-1, data = OrchardSprays)
summary(spray.aov1)
summary(spray.aov2.1)
summary(spray.aov2.2)
model.tables(spray.aov1, "effects", se=TRUE)
#Observations
#treatment B was not found to be significantly useful. Treatment C was significant, but not as significant as treatments D-H
#I'm currently not sure how to answer your question but I'll guess
#I think the model.tables function tells you the mean decrease for each treatment?
#So for A: -40.8, B:-37.8, C:-20.17, D:10.42, E:17.70, F:23.58, G:23.08, H:44.83
OrchardSprays$treatment = factor(OrchardSprays$treatment, levels=(c("B", "A", "C", "D", "E", "F", "G", "H")))
spray.aov2.3 = lm(decrease~treatment, data=OrchardSprays)
summary(spray.aov2.3)
TukeyHSD(spray.aov1)
#Note to self: Multiply tails
(0.95*0.95*0.05)*3
#0.135, so 13.5% chance of making a type 1 error?
data("CO2")
head(CO2, 10)
boxplot(uptake~Treatment*Type, data=CO2, xlab= "Treatment", ylab="decrease mean (effect size)")
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)
model.tables(CO2.aov, "mean", se=TRUE)
model.tables(CO2.aov, "mean", se=TRUE)$tables
model.tables(CO2.aov, "mean", se=TRUE)$tables$Treatment
model.tables(CO2.aov, "mean", se=TRUE)$tables$Type
#The effect is somewhere around 3 or 4 for all of them excecpt Mississippi, which is closer to 7. Further investigation of the data might look at this
treatment.aov1= lm(uptake~Treatment+Type, data = CO2)
summary(treatment.aov1)
# If we start from the not chilled treatment, and change to chilled, we will see a change in -6. If we start at Quebec and change to Mississippi, we will see a change of -12
#Both results were significant
TukeyHSD(CO2.aov)
kruskal.test(uptake~Treatment, data = CO2)
library(dunn.test)
dunn.test(CO2$uptake, CO2$Treatment, method="bonferroni")
#I'm loving this little quadrant graph in the output
dunn.test(airquality$Ozone, airquality$Month, kw=TRUE, method="bonferroni")
dunn.test(airquality$Ozone, airquality$Month, kw=TRUE, method="hs")
dunn.test(airquality$Ozone, airquality$Month, kw=TRUE, method="hs")
#I was wondering what would happen with a Three way anova. Interpreting that output would be a nightmare
