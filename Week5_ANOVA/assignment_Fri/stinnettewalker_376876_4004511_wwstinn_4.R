data(CO2)
head(CO2, 10)
str(CO2)

### Exercise 1 ###
CO2.aov = aov(uptake~Treatment + Type, data = CO2)
summary(CO2.aov)
model.tables(CO2.aov, "effects", se=TRUE)
# The summary table shows that there is a significant difference between the two treatments (p-value<0.0001)
# The summary table shows that there is a significant difference between the two types (p-value<0.0001)
# The model table shows that the effect size for nonchilled is 3.43, while the effect size for chilled is -3.43. 
# This means that the average effect for just nonchilled is 3.43 greater than the average of both treatments, while the effect for just chilled is 3.43 less than the average of both treatments.
# The model table shows that the effect size for Quebec is 6.33, while the effect size for Mississippi is -6.33
# This means that the average effect for just Quebec is 6.33 greater than the average of both types, while the effect for just Mississippi is 6.33 less than the average of both types

# Another way to analyze the data is to look at the linear model and compare to the average.
mean.CO2 = mean(CO2$uptake)
mean.CO2
# The grand mean uptake is 27.2131

CO2.aov.1 = lm(CO2$uptake~CO2$Treatment-1)
summary(CO2.aov.1)
nonchilled.estimate=30.643
chilled.estimate=23.783
# The summary indicates that the estimate for nonchilled and chilled are 30.643 and 23.783, respectively.
nonchilled.estimate-mean.CO2
chilled.estimate-mean.CO2
# The calculation shows that the effect size for nonchilled is 3.43, while the effect size for chilled is -3.43 

CO2.aov.2 = lm(CO2$uptake~CO2$Type-1)
summary(CO2.aov.2)
Quebec.estimate=33.543
Mississippi.estimate=20.883
# The summary indicates that the estimate for Quebec and Mississippi are 33.543 and 20.883, respectively.
Quebec.estimate-mean.CO2
Mississippi.estimate-mean.CO2
# The Calculation shows that the effect size for Quebec is 6.33, while the effect size for Mississippi is -6.33

### Exercise 2 ###
# Two-way ANOVA using the lm() function
CO2.lm = lm(CO2$uptake~CO2$Treatment + CO2$Type)
summary(CO2.lm)
# The summary indicates that -6.8595238 is the mean difference between chilled and nonchilled, which is statistically significant (p-value=0.0002)
# The summary indicates that -12.6595238 is the mean difference between Quebec and Mississippi, which is statistically significant (p-value=3.68e-10)

CO2.lm.1 = lm(CO2$uptake~CO2$Treatment*CO2$Type)
summary(CO2.lm.1)
# For Quebec plants, there is a -3.581 mean difference between chilled and nonchilled plants, but this is not statistically significant (p=0.151)
# For nonchilled plants, uptake is -9.3809524 higher for Quebec plants than Mississippii plants, which is significant (p-value=0.0003)
# The mean difference from nonchilled to chilled in Mississippi plants compared to Quebec plants is -6.557, although this interaction between treatment and type is not significant (p=0.0642)
