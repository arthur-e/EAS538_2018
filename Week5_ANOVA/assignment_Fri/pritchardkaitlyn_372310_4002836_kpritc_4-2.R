### EXERCISE 1 ###
data(CO2)
head(CO2, 10)
str(CO2)
mean(CO2$uptake)
# mean = 27.2131

## Effect size of plant individual (Plant)
CO2.aov1=aov(CO2$uptake~CO2$Plant)
summary(CO2.aov1)
model.tables(CO2.aov1, "effects", se=TRUE)
# The table gives us the difference in uptake for each plant individual from the mean uptake (27.2131)
# This is correct because if you run lm() on the data, the lm() estimate for each plant is the same as the effect size + the mean uptake...
# For example, model.tables gives us effect size of 6.015 for QN1. Running (lm) gives us an estimate of 33.23 for QN1, which is 6.015 + 27.2131.
# Summary(aov()) = anova(lm())
CO2.aov1a=lm(CO2$uptake~CO2$Plant-1)
summary(CO2.aov1a)
  
## Effect size of origin of the plant (Type)
CO2.aov2=aov(CO2$uptake~CO2$Type)
summary(CO2.aov2)
model.tables(CO2.aov2, "effects", se=TRUE)
# The table tells us that the effect of a plant being from Quebec on the uptake mean is 6.33 and a plant from Mississippi is -6.33
# This is supported by running lm(), where the estimate for Mississippi is -12.66 and the intercept (Quebec) is 33.543. 
# Mississippi is (6.33*2) off of Quebec uptake and Quebec's effect is 6.33 above the uptake mean while Mississippi is 6.33 below the uptake mean
CO2.aov2a=lm(CO2$uptake~CO2$Type-1)
summary(CO2.aov2a)

## Effect size of treatment (chilled or nonchilled)
CO2.aov3=aov(CO2$uptake~CO2$Treatment)
summary(CO2.aov3)
model.tables(CO2.aov3, "effects", se=TRUE)
# This tells us that the effect of nonchilled treatment on the uptake mean is 3.43 and the effect of chilled is -3.43
# If we run lm(), chilled treatment is 6.86 less than the intercept of nonchilled treatment (30.643). 
# 30.643 for chilled is 6.86 more than uptake mean
CO2.aov3a=lm(CO2$uptake~CO2$Treatment)
summary(CO2.aov3a)

### EXERCISE 2 ###
# We assume that nonchilled treatment and plant type Quebec are what the table is comparing against
CO2.2way=lm(CO2$uptake~CO2$Treatment + CO2$Type)
summary(CO2.2way)
# The mean difference in uptake between the chilled and nonchilled treatment is -6.860 (nonchilled is 6.860 higher than chilled)
# The mean difference in uptake between plants originating in Quebec v. Mississippi is -12.660 (Quebec is 12.660 higher than Mississippi)
# Since the p value of Treatment is <0.05, applying the chilled treatment does have a significant effect on uptake
# Since the p value of Type is <0.05, sourcing your plant from Mississippi does have a signficiant effect on uptake

CO2.2wayb=lm(CO2$uptake~CO2$Treatment*CO2$Type)
summary(CO2.2wayb)
# The interaction term of -6.557 tells up this is the change of uptake from nonchilled to chilled in Quebec v. Mississippi plants
# Since the p value for the interaction factor is >0.05, the level of impact of the chilled treatment on uptake does not depend on the impact of plant origin (Quebec v. MI)
# For plants from Quebec, the uptake for nonchilled treatment is 3.581 higher than chilled
# Since p>0.05, there is no significant difference in uptake for chilled v. nonchilled Quebec plants
# Nonchilled plants originating from Quebec have an uptake 9.381 higher than nonchilled plants from Mississippi
# Since p<0.05, there is a significant difference between nonchilled plants from Quebec v. Mississippi 

CO2$Type=factor(CO2$Type, levels=c("Mississippi", "Quebec"))
CO2.test=lm(CO2$uptake~CO2$Treatment*CO2$Type)
summary(CO2.test)
# Reordering the data to make the intercept nonchilled Mississippi shows...
# There is a significant difference in uptake between chilled and nonchilled MS plants (p<0.05)...chilled plant uptake is 10.138 less than nonchilled
# There is a significant difference in uptake between nonchilled MS and Quebec plants (p<0.05)
# Confirms that the interaction estimate is 6.557 and that moving from a nonchilled MS plant to chilled Quebec plant results in an uptake that's 6.557 higher
# Since p of the interaction factor is >0.05, the level of impact of the chilled treatment on uptake does not depend on the plant origin