#Exercise 1
data("CO2")
CO2.aov1.1=aov(uptake~Treatment,data=CO2)
CO2.aov1.2=aov(uptake~Type,data=CO2)
#1.1, 1.2 are One way ANOVAs 
CO2.aov1.3=aov(uptake~Treatment+Type,data=CO2)
CO2.aov1.4=aov(uptake~Treatment*Type,data=CO2)
#1.3, 1.4 are TWO way ANOVAs
summary(CO2.aov1.1)
summary(CO2.aov1.2)
summary(CO2.aov1.3)
summary(CO2.aov1.4)
#p-value of treatments is smaller than 0.05, so we can reject the null hypothsis. 
#So, there is significant difference betwwen different treatments
#P-value of types is smaller than 0.05, so we can reject the null hypothsis. 
#So, there is signicant difference between different types
#P-value of interaction is larger than 0.05, that means types/treatments has no different effect in different treatments/types
A=model.tables(CO2.aov1.4,"effects",se=T)
str(A)
A$tables$Treatment  
#The difference between the mean of nonchilled and grand mean is 3.429762
#The difference between the mean of chilled and grand mean is -3.429762
A$tables$Type 
#The difference between the mean of Quebec and grand mean is  6.329762
#The difference between the mean of Mississippi and grand mean is -6.329762 

A1=model.tables(CO2.aov1.4,"means",se=T)
str(A1)
A1$tables$Treatment
#The mean of nonchilled group is 30.64286
#The mean of chilled group is 23.78333
A1$tables$Type
#The mean of Quebec group is 33.54286
#The mean of Mississippi group is 20.88333 


#Exercise 2
data("CO2")
CO2.lm2.1=lm(uptake~Treatment,data=CO2)
summary(CO2.lm2.1)
#If I build a linear model: y=ax+b, the summary tells me the "b" in my linear model is 30.643, and it is significant different from 0 because the p-value is smaller than 0.05
#The summary also tells me the "a" is -6.860, and it is also significantly different from 0, because the p-value is smaller than 0.05.
CO2.lm2.2=lm(uptake~Type,data=CO2)
#If I build a linear model: y=ax+b, the summary tells me the "b" in my linear model is 33.543, and it is significant different from 0 because the p-value is smaller than 0.05
#The summary also tells me the "a" is -12.660, and it is also significantly different from 0, because the p-value is smaller than 0.05.
summary(CO2.lm2.2)
CO2.lm2.3=lm(uptake~Treatment*Type,data=CO2)
summary(CO2.lm2.3)
#In the interact effect of treatments and types, y=-6.557(x3)-9.381(x2)-3.581(x1)+35.333.
#But in the summary, the estimate of Treatment and interaction are not significant. 
#So, we should test if we can exclude treatment and interaction.
Trea=lm(uptake~Type,data=CO2)
Inter=lm(uptake~Treatment*Type,data=CO2)
B=anova(Trea,Inter)
B #P-value<0.05, so we can not exclude treatment from model

Add=lm(uptake~Treatment+Type,data=CO2)
Inter=lm(uptake~Treatment*Type,dara=CO2)
C=anova(Add,Inter)
C #P-value>0.05, so we can exclude the effect of interactive from model
#So the model should be y=-9.381(x2)-3.581(x1)+35.333
