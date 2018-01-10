#####Exercise 1: Try to use the following data set to execute ANCOVA and make interaction plots 
#This data set contains the fruit weight (Fruit) of different plants with different root depth 
#(root) and different treatment (Grazing).

install.packages("RCurl")
library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)

gz = inner_join(money15, bat14, by="playerID") %>%
  filter(BA != "NA") %>%
  subset(select=c("lgID.x", "salary", "BA"))
head(dat, 15)

#View variable types
str(gz)

#Plot data
ggplot(data=gz, mapping=aes(y=Fruit, x=Root))+
ggplot(data=gz, mapping=aes(y=Fruit, x=Grazing))+

#Test normality

qqline(gz[,"Root"], col="red")
shapiro.test(gz[,"Root"])
#p-value = 0.6104, normal!

qqline(gz[,"Fruit"], col="red")
shapiro.test(gz[,"Fruit"])
#p-value= 0.7798, normal!

##ANCOVA

mod = lm(Fruit~ Root*Grazing, data=gz)
summary(mod)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          -125.173     12.811  -9.771 1.15e-11 ***
#  Root                   23.240      1.531  15.182  < 2e-16 ***
#  GrazingUngrazed        30.806     16.842   1.829   0.0757 .  
#Root:GrazingUngrazed    0.756      2.354   0.321   0.7500    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 6.831 on 36 degrees of freedom
#Multiple R-squared:  0.9293,	Adjusted R-squared:  0.9234 
#F-statistic: 157.6 on 3 and 36 DF,  p-value: < 2.2e-16
anova(mod)

interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Fruit", title="Estimated Coefficient of Grazing on Fruit")


#####Exercise 2

#Interaction terms

##Part 1

data("airquality")
head(airquality, 15)

mod = lm(Temp~Wind, data=airquality)
summary(mod)

plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")

##Part 2, Wind + Solar

mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)
#Multiple R-squared:  0.5007,	Adjusted R-squared:  0.4918 
#F-statistic: 56.65 on 2 and 113 DF,  p-value: < 2.2e-16

mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)
#Multiple R-squared:  0.5668,	Adjusted R-squared:  0.5552 
#F-statistic: 48.84 on 3 and 112 DF,  p-value: < 2.2e-16

#The addition of the interaction term appears to weaken the effect of Ozone because 
#Ozone and Wind are highly correlated (P = 6.93e-05). Because knowing this interaction 
#is important, I would choose model 2 over model 1, which does not reveal this high 
#correlation and makes Wind appear insignificant.

##PART 2

mod3 = lm(Temp~Wind * Solar.R, data=airquality)
summary(mod3)
#Multiple R-squared:  0.2692,	Adjusted R-squared:  0.2538 
#F-statistic: 17.44 on 3 and 142 DF,  p-value: 1.077e-09

interplot(mod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar")

interplot(mod3, var1="Solar.R", var2="Wind")+
  labs(x="Ozone", y="coefficient for Wind")

#The interaction plots reveals that the correlation between Wind and Solar is quite weak.
#The data confirms this, Wind:Solar.R P-value = 0.7537