#Ho Hsieh_wk5_lab4

#Exercise 1####
CO2.aov.1=aov(uptake~Treatment*Type, data=CO2) #analyze interaction#
summary(CO2.aov.1)

mx=model.tables(CO2.aov.1, "effects", se=TRUE)
my=model.tables(CO2.aov.1, "means", se=TRUE) 
str(mx)
mx$tables$Treatment 
#the difference between the mean of nonchilled treatment and the grand mean is: 3.429762
#the difference between the mean of chilled treatment and the grand mean is: -3.429762
mx$tables$Type
#the difference between the mean of Quebec and the grand mean is: 6.329762
#the difference between the mean of Mississippi and the grand mean is: -6.329762 

my$tables$Treatment
#the mean of nonchilled treatment is 30.64286
#the mean of chilled treatment is 23.78333 
my$tables$Type
#the mean of Quebec is 33.54286
#the mean of Mississippi is 20.88333



#Exercise 2####
x = lm(uptake~Treatment, data = CO2)
y = lm(uptake~Type, data = CO2)
z = lm(uptake~Type*Treatment, data = CO2)

summary(x)
#the model: y=30.643-6.860x; Both p-values (<0.05) shows that the results are significant. 
#Therefore, it means that both Treatment "nonchilled" and "chilled" have significant impact on the uptake.
#when the treatment is "nonchilled" (x=0), the uptake(y) is 30.643
#when the treatment is "chilled" (x=1), the uptake(y) is 30.643-6.80=23.783

summary(y)
#the model: y=33.543-12.660x; Both p-values (<0.05) shows that the results are significant. 
#Therefore, it means that both Type "Quebec" and "Mississippi" have significant impact on the uptake.
#when the treatment is "Quebec" (x=0), the uptake(y) is 33.543
#when the treatment is "Mississippi" (x=1), the uptake(y) is 30.643-12.660=17.983

summary(z)
#the model: y=35.333-9.381(x1)-3.581(x2)-6.557(x3); 
#The p-values for type "Quebec" and treatment "nonchilled" shows that the impact of "Quebec" and "nonchilled" on uptake is significant(<0.05). 
#The p-values for type "Mississippi" shows that the impact of "Mississippi" on uptake is significant(<0.05). 
#The p-value for treatment "chilled" shows the impact of "chilled" on uptake is not significant(>0.05).
#The p-value for the interaction between treatment and type show that the impact of interaction on uptake is not significant(>0.05).

#Then, we need to decide what variables to put into the model.
lmx1 = lm(uptake ~ Type, CO2)
lmx2 = lm(uptake ~ Type * Treatment, CO2)
m1 = anova(lmx1,lmx2) 
m1 #p<0.05

lmx3 = lm(uptake ~ Type + Treatment, CO2)
lmx4 = lm(uptake ~ Type * Treatment, CO2)
m2 = anova(lmx3,lmx4) 
m2 #p>0.05
#Therefore, the results suggest that the model should be y=35.333-9.381(x1)-3.581(x2) because x3 (interaction) have no significant impact on the value of y (uptake)

#Therefore,
#when the type is "Quebec" (x1=0) and the treatment is nonchilled (x2=0), the uptake(y) is 35.333
#when the type is "Mississippi" (x1=1) and the treatment is nonchilled (x2=0), the uptake(y) is 35.333-9.381[change type]=25.952
#when the type is "Quebec" (x1=0) and the treatment is chilled (x2=1), the uptake(y) is 35.333-3.581[change treatment]=31.752
#when the type is "Mississippi" (x1=1) and the treatment is chilled (x2=1), the uptake(y) is 35.333-9.381[change type]-3.581[change treatment]-0[no interaction]=22.371
