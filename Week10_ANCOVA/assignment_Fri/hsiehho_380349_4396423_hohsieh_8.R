#Ho Hsieh_NRE538_Lab8

#Exercise 1####
library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)

qqnorm(gz[,"Root"]) 
qqline(gz[,"Root"], col="red")
shapiro.test(gz[,"Root"]) 
#p-value = 0.6104--> can't reject the H0, that the distribution is not different from normal distributuin
#-->(the distribution is not different from normal distribution)

qqnorm(gz[,"Fruit"]) 
qqline(gz[,"Fruit"], col="red")
shapiro.test(gz[,"Fruit"]) 
#p-value = 0.7798--> can't reject the H0, that the distribution is not different from normal distributuin
#-->(the distribution is not different from normal distribution)

gz.ung=subset(gz, Grazing=="Ungrazed")
plot(gz.ung$Root, gz.ung$Fruit)
abline(lm(Fruit~Root, data = gz.ung), col="green")

gz.g=subset(gz, Grazing=="Grazed")
plot(gz.g$Root, gz.g$Fruit)
abline(lm(Fruit~Root, data = gz.g), col="red")

#Ex1_ANCOVA####
lm.gz=lm(Fruit~Root*Grazing, data = gz)
summary(lm.gz)
#interpretation:
#(Estimate)
#the estimate for the Intercept is -125.173, 
  #which is the weight of the fruit for "Grazed" treatment when Root=0, 
  #and the intercept for the "Grazed" line
  #the p-value <2e-16 < 0.05, which means the intercept is significantly different from 0

#the estimate for the Root is 23.240, 
  #which is the effect of Root length on the Fruit weight for the "Grazed" category, 
  #as well as the slope of the "Grazed" line.
  #the p-value <2e-16 < 0.05, which means this effect/the slope is significantly different from 0

#The estimate for the "GrazingUngrazed" is 30.806, 
  #which is the difference in intercept between "Grazed"(0) and "Ungrazed"(1) category.
  #That is, the intercpet for the "Ungrazed"(1) line is -125.173 + 30.806*1 = -94.367
  #The p-value = 0.0757 > 0.05, which means the effect of changing the treatment to "Ungrazed" on Fruit weight is not significant
    #Or, the intercept of the "Ungrazed" treatment is not significantly different from the "Grazed" treatment

#The estimate for the "Root:GrazingUngrazed" is 0.756, which is the effect of Treatment on the effect of Root length on Fruit weight
  #That is, when the treatment changes from "Grazed"(0) to "Ungrazed"(1), the effect of Root on Fruit (the slope of the "Ungrazed" line) is 23.240 + 0.756*1 = 23.996
  #However, the p-value is 0.7500 > 0.05, which means the interaction effect is not different from no effect
  #that is, the effect of Root length on Fruit weight (the slope of the line) is not different between "Grazed" and "Ungrazed" treatment

#In conclusion, the results tell us that the Root has an effect on the Fruit weight that every unit increase of Root increases 23.240 unit of Fruit weight
#The treatment (Ungrazed or Grazed) has no significant effect on the Fruit weight. 
#And the effect of Root on Fruit is not significantly different between Grazed and Ungrazed treatment.

#The Adjusted R-squared:  0.9234 means the model explains 92.34% of the variance in the data which indicates the model fit the data pretty well
#F-statistic: 157.6 on 3 and 36 DF,  p-value: < 2.2e-16 < 0.05 means that the model is significantly different from "no model"/randomness
  #Or, the probability that "no model" (the intercept) will have the result of the model is < 2.2e-16



#Ex1_Interaction Plots####
library(interplot)
interplot(m=lm.gz, var1="Root", var2 = "Grazing")+
  labs(x="Treatment", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Treatment")
#the effect of Root on Fruit for Grazing (0): 23.240 (23.240+0.756*0)
#the effect of Root on Fruit for Ungrazing (1): 23.996 (23.240+0.756*1)
#the point for either Ungrazed or Grazed treatment fall into the other's  95% CI 
#which means the differences of the effect of Root on Fruit between Grazed and Ungrazed treatment is not significant


interplot(m=lm.gz, var1 = "Grazing", var2 = "Root")+
  labs(x="Root", y="Estimated coefficient for Treatments", title="Estimated Coefficient of Treatment on Root")
#The differences of the effect of Treatments (Grazed or Ungrazed) on Fruit for different Root lengths 
#grey area: the CI
#interpretation: the slope of the line is the interaction term:0.756
#the effect of Treatments on Fruit weight increases as the Root length increases
  #Root=0, the effect of the Ungrazed treatment (the estimate) on Fruit weight is 30.806; 
  #Root=1, the effect of the Ungrazed treatment (the estimate) on Fruit weight is 30.806+0.756*1;
  #Root=2, the effect of the Ungrazed treatment (the estimate) on Fruit weight is 30.806+0.756*2;
#However, this change/difference caused by Root length in the effect of treatments on Fruit weight, or the interaction effect, is NOT significant.
#(we are able to draw a horizontal line in the grey area)



#Exercise 2####

#Ex2-1_F-test####
data("airquality")
head(airquality, 15)

mod1 = lm(Temp~Wind+Ozone, data = airquality)
summary(mod1)
mod2 = lm(Temp~Wind*Ozone, data = airquality)
summary(mod2)

anova(mod1, mod2)
#The result of the F-test shows that the P-value = 6.927e-05 < 0.05
#which is the probability that mod2 would explain as much variance as mod1 (the probability that mod2 is the same as mod1)

#the small probalility means the amount of variances that cannot be explained by the two models (RSS) are significantly different
#with mod1 having more variances that it cannot explain (RSS)
#therefore, mod2 can explained more variance than mod1, and is the better model




#Ex2-2_Interaction Plots####
mod3=lm(Temp~Wind*Solar.R, data = airquality)
summary(mod3)

#Temp = ??0 + ??.wind*Wind + ??.solar.r*Solar.R + ??.inter*(Wind*Solar.R)(Interaction)
#Temp = 83.5922238 + (-1.0384133)*Wind + 0.0328498*Solar.R + (-0.0006492)*(Wind*Solar.R)

###in terms of the effect of Solar.R on ??.wind
#Temp = 83.5922238 + (-1.0384133 + (-0.0006492)*Solar.R)*Wind + 0.0328498*Solar.R  

###in terms of the effect of Wind on ??.solar.r
#Temp = 83.5922238 + (-1.0384133)*Wind + (0.0328498 + (-0.0006492)*Wind)*Solar.R 

#Considering the p-values, only the intercept and the coefficient of Wind are significant
#The coefficient of Solar.R and the coefficient of the interaction are not significant
#--> Temp = ??0 + ??.wind*Wind
#--> Temp = 83.5922238 + (-1.0384133)*Wind


interplot(mod3, var1 = "Solar.R", var2 = "Wind")+
  labs(x="Solar.R", y="coefficient for Wind")
#The result shows that as the Solar.R increases, 
#the effect of Wind on Temperature (= the coefficient of Wind = ??.wind) decreases with a slope of -0.0006492
#that is,  ??.wind (the coefficient of Wind) = -1.0384133-0.0006492*Solar.R
#However, as we can see in the graph, the interaction effect of Solar.R on Wind on Temperature is not significant
#since we can draw a horizontal line within the grey area (Confidence Interval)

interplot(m=mod3, var1="Wind", var2 = "Solar.R")+
  labs(x="Wind", y="Estimated coefficient for Solar.R")
#The result shows that as the Wind increases, 
#the effect of Solar.R on Temperature (= the coefficient of Solar.R = ??.solar.r) decreases with a slope of -0.0006492
#that is, ??.solar.r (the coefficient of Solar.R) = 0.0328498-0.0006492*Wind
#However, as we can see in the graph, the interaction effect of Wind on Solar.R on Temperature is not significant
#since we can draw a horizontal line within the grey area (Confidence Interval)
