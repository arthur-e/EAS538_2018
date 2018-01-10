setwd("D:/UM/SNRE/Winter 2017/NRE 538 Statistics/Week 10 Lab")
install.packages("RCurl")
library(RCurl)
gz= read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")


#Ex 1 Try to use the following data set to execute ANCOVA and make interaction plots 
#This data set contains the fruit weight (Fruit) of different plants with different root depth (root) and different treatment (Grazing).
graze=subset(gz,Grazing=="Grazed")
ungraze=subset(gz,Grazing=="Ungrazed")

plot(Fruit~Root, data=graze)
plot(Fruit~Root, data=ungraze)

hist(gz$Root)
qqnorm(gz$Root)
qqline(gz$Root,col="Red")
shapiro.test(gz$Root) #The root length is normally distributed

hist(gz$Fruit)
qqnorm(gz$Fruit)
qqline(gz$Fruit,col="Red")
shapiro.test(gz$Fruit) #The fruit weight is normally distributed

summary(lm(Fruit~Root,data=graze))
summary(lm(Fruit~Root,data=ungraze))

mod=lm(Fruit~Root*Grazing,data=gz)
summary(mod)
#Intercept (fruit weight of grazed plant when root length=0) differs significantly from 0
#Root length has significantly positive relationship with fruit weight for grazed plants. 
#One unit increase in root lenght, 23.24 unit increase in fruit weight
#No significant difference between fruit weight of grazed and ungrazed plants when root length=0 (no significant difference in intercepts of two plots, although the intercept for ungrazed plants is 30.8 units higher than grazed plants)
#Root length's influence on fruit weight is not significantly different between grazed and ungrazed plants 
#(no significant difference in slopes of two plots, although the slope for ungrazed plants is higher than grazed plants by 0.76)
#This also means that there's no significant interaction between grazing and root length

library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root Length", title="Estimated Coefficient of Root Length on Grazing")
#The mean of estimates fall into each other's CI, so is no significant difference between the coefficient for root lenght of grazed and ungrazed plants
#This also means that there's no significant interaction between grazing and root length

interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Root Length")
#Positive intercept shows that ungrazed plant have heavier fruit at first
#The coefficient for grazing remains positive, meaning the difference in fruit weight keeps increasing as root length increases
#Can the interplot show if the interaction (slope of interplot?) is significant or not? I don't think it can...
#I can tell it's not significant from previous analysis

##### Oscar: Yes, you can tell if the interaction term is significant from this interactino plot. 
#####        In this plot, if the mean (the black dot) of one regression coefficient of one group is in the 95% CI of the other group, it is not significant. 
#####        For example, the mean of the regression coefficient of grazed treatment (~23) is in the 95% of that of ungrazed group. 
#####        The interaction term is not significant. 

#Ex2-1 Use F-test to compare mod.1 and mod.2. Explain why you chose one over another to explain the data.
mod1 = lm(Temp~Wind + Ozone, data=airquality)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
anova(mod1,mod2)
#I would choose mod2
#The p-value<0.05, means that the RSS in mod2 is significantly lower than mod1, and mod2 explain significantly more variation

#Ex2-2 Plot two interaction plots with interplot() for the model with wind and solar radiation as the independent variables. Interpret the two interaction plots.
mod3=lm(Temp~Solar.R*Wind,data=airquality)
data("airquality")

summary(mod3)
interplot(m=mod3, var1="Wind", var2="Solar.R")+
  labs(x="Solar Radiation", y="Coefficient for Wind")
#As solar radiation increases, wind's negative relationship to temperature increases (becomes more negative)
#The effect of solar radiation on coefficient of wind is not significant

interplot(m=mod3, var1="Solar.R", var2="Wind")+
  labs(x="Wind", y="Coefficient for Solar Radiation")
#As wind increases, solar radiation's positive relationship to temperature decreases (the effect becomes weaker)
#The effect of wind to coefficient of solar radiation is not significant
