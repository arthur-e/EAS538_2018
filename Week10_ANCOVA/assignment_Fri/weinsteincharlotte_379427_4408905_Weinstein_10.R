### Lab 10
### Charlotte Weinstein
### Mar 21, 2017

## Exercise 1

#load libraries
library(RCurl)
library(dplyr)
library(ggplot2)

gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
#read in data

str(gz)
#Root and Fruit are both continuous, while Grazing is categorical

ggplot(data=gz, mapping=aes(x=Root, y=Fruit, color=factor(Grazing)))+
  geom_point()
#plot and visually inspect the data. Root depth & fruit weight appear to be fairly correlated, with Grazed vs. Ungrazed influencing the intercept.

ggplot(data=gz, mapping=aes(x=Root)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
#Root data seems like it could be slightly normal

ggplot(data=gz, mapping=aes(x=Fruit)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
#Fruit data also seems like it could be slightly normal

#assess normality using qqplot:
qqnorm(gz[,"Root"])
qqline(gz[,"Root"], col="red")
#Root data doesn't appear to be normal, based on the qqplot

qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"], col="red")
#Fruit qqplot appears to be more normal

#assess normality using Shapiro-Wilke test:
shapiro.test(gz[,"Root"])
#Root data actually passes the test, which is surprising, given the qqplot

shapiro.test(gz[,"Fruit"])
#Fruit data also passes the test. Less surprising.

#No transformation required for either variable.

gz %>%
  filter(Grazing=="Grazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root", y="Fruit", title="Grazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

gz.Grazed = gz %>%
  filter(Grazing=="Grazed")
summary(lm(Fruit~ Root, data=gz.Grazed))
#There does seem to be a significant positive relationship between Root & Fruit for Grazed plants.

gz %>%
  filter(Grazing=="Ungrazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root", y="Fruit", title="Ungrazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

gz.Ungrazed = gz %>%
  filter(Grazing=="Ungrazed")
summary(lm(Fruit~ Root, data=gz.Ungrazed))
#There is also a significant positive relationship between Root & Fruit for Ungrazed plants.

#Define linear model to execute the ANCOVA:
mod = lm(Fruit~ Root*Grazing, data=gz)
summary(mod)
anova(mod)

#From the results, we can see that there is no significant interaction.
#This means that the relationship between root depth and fruit weight is not significantly
#different between grazed and ungrazed plants.

##Generating interaction plots:

library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Grazing")
#This shows that the effect of Root on Fruit is slightly higher in Ungrazed than Grazed.
#However, this difference is not significant.

interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Root")
#This confirms that the interaction term is not significant.


### Exercise 2

#load data, create models
data("airquality")
mod1 = lm(Temp~Wind + Ozone, data=airquality)
mod2 = lm(Temp~Wind * Ozone, data=airquality)

#run f-test:
anova(mod1, mod2)
#Here, we see that the likelihood of mod2 explaining the same amount of variance as mod1 is very small.
#This suggests that mod2 explains significantly more variance than mod1, so we choose mod2.

#Generate interaction plot with wind as independent variable:
interplot(m=mod2, var1="Ozone", var2="Wind")+
  labs(x="Wind", y="Estimated coefficient for Ozone", title="Estimated Coefficient of Ozone on Wind")
#Here, we see that as Wind increases, the coefficient for Ozone increases.
#Because a horizontal line cannot be drawn within the 95% confidence interval (as indicated by the gray area),
#we can say that there is a significant interaction term.


#Generate interaction plot with solar radiation as independent variable:
mod2.1 = lm(Temp~Solar.R * Ozone, data=airquality)
interplot(m=mod2.1, var1="Ozone", var2="Solar.R")+
  labs(x="Solar.R", y="Estimated coefficient for Ozone", title="Estimated Coefficient of Ozone on Solar Radiation")
#Here, we see that as Solar.R increases, the coefficient for Ozone decreases.
#However, because a horizontal line can be drawn within the 95% confidence interval (as indicated by the gray area),
#we the interaction term is not significant.
